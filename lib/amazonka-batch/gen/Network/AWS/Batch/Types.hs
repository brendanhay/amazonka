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
    batchService,

    -- * Errors

    -- * ArrayJobDependency
    ArrayJobDependency (..),

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

    -- * ResourceType
    ResourceType (..),

    -- * RetryAction
    RetryAction (..),

    -- * ArrayProperties
    ArrayProperties (..),
    mkArrayProperties,
    apSize,

    -- * ArrayPropertiesDetail
    ArrayPropertiesDetail (..),
    mkArrayPropertiesDetail,
    apdSize,
    apdStatusSummary,
    apdIndex,

    -- * ArrayPropertiesSummary
    ArrayPropertiesSummary (..),
    mkArrayPropertiesSummary,
    apsSize,
    apsIndex,

    -- * AttemptContainerDetail
    AttemptContainerDetail (..),
    mkAttemptContainerDetail,
    acdNetworkInterfaces,
    acdTaskARN,
    acdContainerInstanceARN,
    acdReason,
    acdLogStreamName,
    acdExitCode,

    -- * AttemptDetail
    AttemptDetail (..),
    mkAttemptDetail,
    adStoppedAt,
    adStartedAt,
    adContainer,
    adStatusReason,

    -- * ComputeEnvironmentDetail
    ComputeEnvironmentDetail (..),
    mkComputeEnvironmentDetail,
    cedStatus,
    cedState,
    cedComputeResources,
    cedStatusReason,
    cedType,
    cedServiceRole,
    cedTags,
    cedComputeEnvironmentName,
    cedComputeEnvironmentARN,
    cedEcsClusterARN,

    -- * ComputeEnvironmentOrder
    ComputeEnvironmentOrder (..),
    mkComputeEnvironmentOrder,
    ceoOrder,
    ceoComputeEnvironment,

    -- * ComputeResource
    ComputeResource (..),
    mkComputeResource,
    crSecurityGroupIds,
    crEc2KeyPair,
    crEc2Configuration,
    crBidPercentage,
    crSpotIAMFleetRole,
    crImageId,
    crLaunchTemplate,
    crDesiredvCPUs,
    crAllocationStrategy,
    crPlacementGroup,
    crTags,
    crType,
    crMinvCPUs,
    crMaxvCPUs,
    crInstanceTypes,
    crSubnets,
    crInstanceRole,

    -- * ComputeResourceUpdate
    ComputeResourceUpdate (..),
    mkComputeResourceUpdate,
    cruMinvCPUs,
    cruMaxvCPUs,
    cruDesiredvCPUs,

    -- * ContainerDetail
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

    -- * ContainerOverrides
    ContainerOverrides (..),
    mkContainerOverrides,
    coCommand,
    coEnvironment,
    coResourceRequirements,
    coInstanceType,
    coMemory,
    coVcpus,

    -- * ContainerProperties
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

    -- * ContainerSummary
    ContainerSummary (..),
    mkContainerSummary,
    csReason,
    csExitCode,

    -- * Device
    Device (..),
    mkDevice,
    dContainerPath,
    dPermissions,
    dHostPath,

    -- * EC2Configuration
    EC2Configuration (..),
    mkEC2Configuration,
    ecImageIdOverride,
    ecImageType,

    -- * EvaluateOnExit
    EvaluateOnExit (..),
    mkEvaluateOnExit,
    eoeOnExitCode,
    eoeOnReason,
    eoeOnStatusReason,
    eoeAction,

    -- * Host
    Host (..),
    mkHost,
    hSourcePath,

    -- * JobDefinition
    JobDefinition (..),
    mkJobDefinition,
    jobStatus,
    jobRetryStrategy,
    jobParameters,
    jobTimeout,
    jobContainerProperties,
    jobNodeProperties,
    jobTags,
    jobJobDefinitionName,
    jobJobDefinitionARN,
    jobRevision,
    jobType,

    -- * JobDependency
    JobDependency (..),
    mkJobDependency,
    jJobId,
    jType,

    -- * JobDetail
    JobDetail (..),
    mkJobDetail,
    jdStoppedAt,
    jdJobARN,
    jdCreatedAt,
    jdRetryStrategy,
    jdAttempts,
    jdStartedAt,
    jdDependsOn,
    jdContainer,
    jdNodeDetails,
    jdParameters,
    jdStatusReason,
    jdArrayProperties,
    jdTimeout,
    jdNodeProperties,
    jdTags,
    jdJobName,
    jdJobId,
    jdJobQueue,
    jdStatus,
    jdJobDefinition,

    -- * JobQueueDetail
    JobQueueDetail (..),
    mkJobQueueDetail,
    jqdStatus,
    jqdStatusReason,
    jqdTags,
    jqdJobQueueName,
    jqdJobQueueARN,
    jqdState,
    jqdPriority,
    jqdComputeEnvironmentOrder,

    -- * JobSummary
    JobSummary (..),
    mkJobSummary,
    jsStoppedAt,
    jsStatus,
    jsJobARN,
    jsCreatedAt,
    jsStartedAt,
    jsContainer,
    jsStatusReason,
    jsArrayProperties,
    jsNodeProperties,
    jsJobId,
    jsJobName,

    -- * JobTimeout
    JobTimeout (..),
    mkJobTimeout,
    jtAttemptDurationSeconds,

    -- * KeyValuePair
    KeyValuePair (..),
    mkKeyValuePair,
    kvpValue,
    kvpName,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification (..),
    mkLaunchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- * LinuxParameters
    LinuxParameters (..),
    mkLinuxParameters,
    lpSharedMemorySize,
    lpInitProcessEnabled,
    lpTmpfs,
    lpSwappiness,
    lpDevices,
    lpMaxSwap,

    -- * LogConfiguration
    LogConfiguration (..),
    mkLogConfiguration,
    lcOptions,
    lcSecretOptions,
    lcLogDriver,

    -- * MountPoint
    MountPoint (..),
    mkMountPoint,
    mpContainerPath,
    mpSourceVolume,
    mpReadOnly,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIpv6Address,
    niPrivateIPv4Address,
    niAttachmentId,

    -- * NodeDetails
    NodeDetails (..),
    mkNodeDetails,
    ndNodeIndex,
    ndIsMainNode,

    -- * NodeOverrides
    NodeOverrides (..),
    mkNodeOverrides,
    noNumNodes,
    noNodePropertyOverrides,

    -- * NodeProperties
    NodeProperties (..),
    mkNodeProperties,
    npNumNodes,
    npMainNode,
    npNodeRangeProperties,

    -- * NodePropertiesSummary
    NodePropertiesSummary (..),
    mkNodePropertiesSummary,
    npsNumNodes,
    npsNodeIndex,
    npsIsMainNode,

    -- * NodePropertyOverride
    NodePropertyOverride (..),
    mkNodePropertyOverride,
    npoContainerOverrides,
    npoTargetNodes,

    -- * NodeRangeProperty
    NodeRangeProperty (..),
    mkNodeRangeProperty,
    nrpContainer,
    nrpTargetNodes,

    -- * ResourceRequirement
    ResourceRequirement (..),
    mkResourceRequirement,
    rrValue,
    rrType,

    -- * RetryStrategy
    RetryStrategy (..),
    mkRetryStrategy,
    rsEvaluateOnExit,
    rsAttempts,

    -- * Secret
    Secret (..),
    mkSecret,
    sName,
    sValueFrom,

    -- * Tmpfs
    Tmpfs (..),
    mkTmpfs,
    tMountOptions,
    tContainerPath,
    tSize,

    -- * Ulimit
    Ulimit (..),
    mkUlimit,
    uHardLimit,
    uName,
    uSoftLimit,

    -- * Volume
    Volume (..),
    mkVolume,
    vName,
    vHost,
  )
where

import Network.AWS.Batch.Types.ArrayJobDependency
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
import Network.AWS.Batch.Types.EC2Configuration
import Network.AWS.Batch.Types.EvaluateOnExit
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
import Network.AWS.Batch.Types.LaunchTemplateSpecification
import Network.AWS.Batch.Types.LinuxParameters
import Network.AWS.Batch.Types.LogConfiguration
import Network.AWS.Batch.Types.LogDriver
import Network.AWS.Batch.Types.MountPoint
import Network.AWS.Batch.Types.NetworkInterface
import Network.AWS.Batch.Types.NodeDetails
import Network.AWS.Batch.Types.NodeOverrides
import Network.AWS.Batch.Types.NodeProperties
import Network.AWS.Batch.Types.NodePropertiesSummary
import Network.AWS.Batch.Types.NodePropertyOverride
import Network.AWS.Batch.Types.NodeRangeProperty
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Batch.Types.ResourceType
import Network.AWS.Batch.Types.RetryAction
import Network.AWS.Batch.Types.RetryStrategy
import Network.AWS.Batch.Types.Secret
import Network.AWS.Batch.Types.Tmpfs
import Network.AWS.Batch.Types.Ulimit
import Network.AWS.Batch.Types.Volume
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-08-10@ of the Amazon Batch SDK configuration.
batchService :: Lude.Service
batchService =
  Lude.Service
    { Lude._svcAbbrev = "Batch",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "batch",
      Lude._svcVersion = "2016-08-10",
      Lude._svcEndpoint = Lude.defaultEndpoint batchService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "Batch",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
