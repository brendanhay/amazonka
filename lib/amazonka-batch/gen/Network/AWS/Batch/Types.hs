{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types
  ( -- * Service Configuration
    batch,

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
    ArrayProperties,
    arrayProperties,
    apSize,

    -- * ArrayPropertiesDetail
    ArrayPropertiesDetail,
    arrayPropertiesDetail,
    apdSize,
    apdStatusSummary,
    apdIndex,

    -- * ArrayPropertiesSummary
    ArrayPropertiesSummary,
    arrayPropertiesSummary,
    apsSize,
    apsIndex,

    -- * AttemptContainerDetail
    AttemptContainerDetail,
    attemptContainerDetail,
    acdNetworkInterfaces,
    acdTaskARN,
    acdContainerInstanceARN,
    acdReason,
    acdLogStreamName,
    acdExitCode,

    -- * AttemptDetail
    AttemptDetail,
    attemptDetail,
    adStoppedAt,
    adStartedAt,
    adContainer,
    adStatusReason,

    -- * ComputeEnvironmentDetail
    ComputeEnvironmentDetail,
    computeEnvironmentDetail,
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
    ComputeEnvironmentOrder,
    computeEnvironmentOrder,
    ceoOrder,
    ceoComputeEnvironment,

    -- * ComputeResource
    ComputeResource,
    computeResource,
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
    ComputeResourceUpdate,
    computeResourceUpdate,
    cruMinvCPUs,
    cruMaxvCPUs,
    cruDesiredvCPUs,

    -- * ContainerDetail
    ContainerDetail,
    containerDetail,
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
    ContainerOverrides,
    containerOverrides,
    coCommand,
    coEnvironment,
    coResourceRequirements,
    coInstanceType,
    coMemory,
    coVcpus,

    -- * ContainerProperties
    ContainerProperties,
    containerProperties,
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
    ContainerSummary,
    containerSummary,
    csReason,
    csExitCode,

    -- * Device
    Device,
    device,
    dContainerPath,
    dPermissions,
    dHostPath,

    -- * EC2Configuration
    EC2Configuration,
    ec2Configuration,
    ecImageIdOverride,
    ecImageType,

    -- * EvaluateOnExit
    EvaluateOnExit,
    evaluateOnExit,
    eoeOnExitCode,
    eoeOnReason,
    eoeOnStatusReason,
    eoeAction,

    -- * Host
    Host,
    host,
    hSourcePath,

    -- * JobDefinition
    JobDefinition,
    jobDefinition,
    jddStatus,
    jddRetryStrategy,
    jddParameters,
    jddTimeout,
    jddContainerProperties,
    jddNodeProperties,
    jddTags,
    jddJobDefinitionName,
    jddJobDefinitionARN,
    jddRevision,
    jddType,

    -- * JobDependency
    JobDependency,
    jobDependency,
    jJobId,
    jType,

    -- * JobDetail
    JobDetail,
    jobDetail,
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
    JobQueueDetail,
    jobQueueDetail,
    jqdStatus,
    jqdStatusReason,
    jqdTags,
    jqdJobQueueName,
    jqdJobQueueARN,
    jqdState,
    jqdPriority,
    jqdComputeEnvironmentOrder,

    -- * JobSummary
    JobSummary,
    jobSummary,
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
    JobTimeout,
    jobTimeout,
    jtAttemptDurationSeconds,

    -- * KeyValuePair
    KeyValuePair,
    keyValuePair,
    kvpValue,
    kvpName,

    -- * LaunchTemplateSpecification
    LaunchTemplateSpecification,
    launchTemplateSpecification,
    ltsLaunchTemplateName,
    ltsLaunchTemplateId,
    ltsVersion,

    -- * LinuxParameters
    LinuxParameters,
    linuxParameters,
    lpSharedMemorySize,
    lpInitProcessEnabled,
    lpTmpfs,
    lpSwappiness,
    lpDevices,
    lpMaxSwap,

    -- * LogConfiguration
    LogConfiguration,
    logConfiguration,
    lcOptions,
    lcSecretOptions,
    lcLogDriver,

    -- * MountPoint
    MountPoint,
    mountPoint,
    mpContainerPath,
    mpSourceVolume,
    mpReadOnly,

    -- * NetworkInterface
    NetworkInterface,
    networkInterface,
    niIpv6Address,
    niPrivateIPv4Address,
    niAttachmentId,

    -- * NodeDetails
    NodeDetails,
    nodeDetails,
    ndNodeIndex,
    ndIsMainNode,

    -- * NodeOverrides
    NodeOverrides,
    nodeOverrides,
    noNumNodes,
    noNodePropertyOverrides,

    -- * NodeProperties
    NodeProperties,
    nodeProperties,
    npNumNodes,
    npMainNode,
    npNodeRangeProperties,

    -- * NodePropertiesSummary
    NodePropertiesSummary,
    nodePropertiesSummary,
    npsNumNodes,
    npsNodeIndex,
    npsIsMainNode,

    -- * NodePropertyOverride
    NodePropertyOverride,
    nodePropertyOverride,
    npoContainerOverrides,
    npoTargetNodes,

    -- * NodeRangeProperty
    NodeRangeProperty,
    nodeRangeProperty,
    nrpContainer,
    nrpTargetNodes,

    -- * ResourceRequirement
    ResourceRequirement,
    resourceRequirement,
    rrValue,
    rrType,

    -- * RetryStrategy
    RetryStrategy,
    retryStrategy,
    rsEvaluateOnExit,
    rsAttempts,

    -- * Secret
    Secret,
    secret,
    sName,
    sValueFrom,

    -- * Tmpfs
    Tmpfs,
    tmpfs,
    tMountOptions,
    tContainerPath,
    tSize,

    -- * Ulimit
    Ulimit,
    ulimit,
    uHardLimit,
    uName,
    uSoftLimit,

    -- * Volume
    Volume,
    volume,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-08-10@ of the Amazon Batch SDK configuration.
batch :: Service
batch =
  Service
    { _svcAbbrev = "Batch",
      _svcSigner = v4,
      _svcPrefix = "batch",
      _svcVersion = "2016-08-10",
      _svcEndpoint = defaultEndpoint batch,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Batch",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
