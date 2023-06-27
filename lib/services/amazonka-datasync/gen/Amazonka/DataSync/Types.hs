{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataSync.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalException,
    _InvalidRequestException,

    -- * AgentStatus
    AgentStatus (..),

    -- * Atime
    Atime (..),

    -- * DiscoveryJobStatus
    DiscoveryJobStatus (..),

    -- * DiscoveryResourceFilter
    DiscoveryResourceFilter (..),

    -- * DiscoveryResourceType
    DiscoveryResourceType (..),

    -- * DiscoverySystemType
    DiscoverySystemType (..),

    -- * EfsInTransitEncryption
    EfsInTransitEncryption (..),

    -- * EndpointType
    EndpointType (..),

    -- * FilterType
    FilterType (..),

    -- * Gid
    Gid (..),

    -- * HdfsAuthenticationType
    HdfsAuthenticationType (..),

    -- * HdfsDataTransferProtection
    HdfsDataTransferProtection (..),

    -- * HdfsRpcProtection
    HdfsRpcProtection (..),

    -- * LocationFilterName
    LocationFilterName (..),

    -- * LogLevel
    LogLevel (..),

    -- * Mtime
    Mtime (..),

    -- * NfsVersion
    NfsVersion (..),

    -- * ObjectStorageServerProtocol
    ObjectStorageServerProtocol (..),

    -- * ObjectTags
    ObjectTags (..),

    -- * Operator
    Operator (..),

    -- * OverwriteMode
    OverwriteMode (..),

    -- * PhaseStatus
    PhaseStatus (..),

    -- * PosixPermissions
    PosixPermissions (..),

    -- * PreserveDeletedFiles
    PreserveDeletedFiles (..),

    -- * PreserveDevices
    PreserveDevices (..),

    -- * RecommendationStatus
    RecommendationStatus (..),

    -- * S3StorageClass
    S3StorageClass (..),

    -- * SmbSecurityDescriptorCopyFlags
    SmbSecurityDescriptorCopyFlags (..),

    -- * SmbVersion
    SmbVersion (..),

    -- * StorageSystemConnectivityStatus
    StorageSystemConnectivityStatus (..),

    -- * TaskExecutionStatus
    TaskExecutionStatus (..),

    -- * TaskFilterName
    TaskFilterName (..),

    -- * TaskQueueing
    TaskQueueing (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * TransferMode
    TransferMode (..),

    -- * Uid
    Uid (..),

    -- * VerifyMode
    VerifyMode (..),

    -- * AgentListEntry
    AgentListEntry (..),
    newAgentListEntry,
    agentListEntry_agentArn,
    agentListEntry_name,
    agentListEntry_status,

    -- * Capacity
    Capacity (..),
    newCapacity,
    capacity_logicalUsed,
    capacity_provisioned,
    capacity_used,

    -- * Credentials
    Credentials (..),
    newCredentials,
    credentials_username,
    credentials_password,

    -- * DiscoveryJobListEntry
    DiscoveryJobListEntry (..),
    newDiscoveryJobListEntry,
    discoveryJobListEntry_discoveryJobArn,
    discoveryJobListEntry_status,

    -- * DiscoveryServerConfiguration
    DiscoveryServerConfiguration (..),
    newDiscoveryServerConfiguration,
    discoveryServerConfiguration_serverPort,
    discoveryServerConfiguration_serverHostname,

    -- * Ec2Config
    Ec2Config (..),
    newEc2Config,
    ec2Config_subnetArn,
    ec2Config_securityGroupArns,

    -- * FilterRule
    FilterRule (..),
    newFilterRule,
    filterRule_filterType,
    filterRule_value,

    -- * FsxProtocol
    FsxProtocol (..),
    newFsxProtocol,
    fsxProtocol_nfs,
    fsxProtocol_smb,

    -- * FsxProtocolNfs
    FsxProtocolNfs (..),
    newFsxProtocolNfs,
    fsxProtocolNfs_mountOptions,

    -- * FsxProtocolSmb
    FsxProtocolSmb (..),
    newFsxProtocolSmb,
    fsxProtocolSmb_domain,
    fsxProtocolSmb_mountOptions,
    fsxProtocolSmb_password,
    fsxProtocolSmb_user,

    -- * HdfsNameNode
    HdfsNameNode (..),
    newHdfsNameNode,
    hdfsNameNode_hostname,
    hdfsNameNode_port,

    -- * IOPS
    IOPS (..),
    newIOPS,
    iops_other,
    iops_read,
    iops_total,
    iops_write,

    -- * Latency
    Latency (..),
    newLatency,
    latency_other,
    latency_read,
    latency_write,

    -- * LocationFilter
    LocationFilter (..),
    newLocationFilter,
    locationFilter_name,
    locationFilter_values,
    locationFilter_operator,

    -- * LocationListEntry
    LocationListEntry (..),
    newLocationListEntry,
    locationListEntry_locationArn,
    locationListEntry_locationUri,

    -- * MaxP95Performance
    MaxP95Performance (..),
    newMaxP95Performance,
    maxP95Performance_iopsOther,
    maxP95Performance_iopsRead,
    maxP95Performance_iopsTotal,
    maxP95Performance_iopsWrite,
    maxP95Performance_latencyOther,
    maxP95Performance_latencyRead,
    maxP95Performance_latencyWrite,
    maxP95Performance_throughputOther,
    maxP95Performance_throughputRead,
    maxP95Performance_throughputTotal,
    maxP95Performance_throughputWrite,

    -- * NetAppONTAPCluster
    NetAppONTAPCluster (..),
    newNetAppONTAPCluster,
    netAppONTAPCluster_cifsShareCount,
    netAppONTAPCluster_clusterBlockStorageLogicalUsed,
    netAppONTAPCluster_clusterBlockStorageSize,
    netAppONTAPCluster_clusterBlockStorageUsed,
    netAppONTAPCluster_clusterName,
    netAppONTAPCluster_maxP95Performance,
    netAppONTAPCluster_nfsExportedVolumes,
    netAppONTAPCluster_recommendationStatus,
    netAppONTAPCluster_recommendations,
    netAppONTAPCluster_resourceId,

    -- * NetAppONTAPSVM
    NetAppONTAPSVM (..),
    newNetAppONTAPSVM,
    netAppONTAPSVM_cifsShareCount,
    netAppONTAPSVM_clusterUuid,
    netAppONTAPSVM_enabledProtocols,
    netAppONTAPSVM_maxP95Performance,
    netAppONTAPSVM_nfsExportedVolumes,
    netAppONTAPSVM_recommendationStatus,
    netAppONTAPSVM_recommendations,
    netAppONTAPSVM_resourceId,
    netAppONTAPSVM_svmName,
    netAppONTAPSVM_totalCapacityProvisioned,
    netAppONTAPSVM_totalCapacityUsed,
    netAppONTAPSVM_totalLogicalCapacityUsed,
    netAppONTAPSVM_totalSnapshotCapacityUsed,

    -- * NetAppONTAPVolume
    NetAppONTAPVolume (..),
    newNetAppONTAPVolume,
    netAppONTAPVolume_capacityProvisioned,
    netAppONTAPVolume_capacityUsed,
    netAppONTAPVolume_cifsShareCount,
    netAppONTAPVolume_logicalCapacityUsed,
    netAppONTAPVolume_maxP95Performance,
    netAppONTAPVolume_nfsExported,
    netAppONTAPVolume_recommendationStatus,
    netAppONTAPVolume_recommendations,
    netAppONTAPVolume_resourceId,
    netAppONTAPVolume_securityStyle,
    netAppONTAPVolume_snapshotCapacityUsed,
    netAppONTAPVolume_svmName,
    netAppONTAPVolume_svmUuid,
    netAppONTAPVolume_volumeName,

    -- * NfsMountOptions
    NfsMountOptions (..),
    newNfsMountOptions,
    nfsMountOptions_version,

    -- * OnPremConfig
    OnPremConfig (..),
    newOnPremConfig,
    onPremConfig_agentArns,

    -- * Options
    Options (..),
    newOptions,
    options_atime,
    options_bytesPerSecond,
    options_gid,
    options_logLevel,
    options_mtime,
    options_objectTags,
    options_overwriteMode,
    options_posixPermissions,
    options_preserveDeletedFiles,
    options_preserveDevices,
    options_securityDescriptorCopyFlags,
    options_taskQueueing,
    options_transferMode,
    options_uid,
    options_verifyMode,

    -- * P95Metrics
    P95Metrics (..),
    newP95Metrics,
    p95Metrics_iops,
    p95Metrics_latency,
    p95Metrics_throughput,

    -- * PrivateLinkConfig
    PrivateLinkConfig (..),
    newPrivateLinkConfig,
    privateLinkConfig_privateLinkEndpoint,
    privateLinkConfig_securityGroupArns,
    privateLinkConfig_subnetArns,
    privateLinkConfig_vpcEndpointId,

    -- * QopConfiguration
    QopConfiguration (..),
    newQopConfiguration,
    qopConfiguration_dataTransferProtection,
    qopConfiguration_rpcProtection,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_estimatedMonthlyStorageCost,
    recommendation_storageConfiguration,
    recommendation_storageType,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_netAppONTAPClusters,
    resourceDetails_netAppONTAPSVMs,
    resourceDetails_netAppONTAPVolumes,

    -- * ResourceMetrics
    ResourceMetrics (..),
    newResourceMetrics,
    resourceMetrics_capacity,
    resourceMetrics_p95Metrics,
    resourceMetrics_resourceId,
    resourceMetrics_resourceType,
    resourceMetrics_timestamp,

    -- * S3Config
    S3Config (..),
    newS3Config,
    s3Config_bucketAccessRoleArn,

    -- * SmbMountOptions
    SmbMountOptions (..),
    newSmbMountOptions,
    smbMountOptions_version,

    -- * StorageSystemListEntry
    StorageSystemListEntry (..),
    newStorageSystemListEntry,
    storageSystemListEntry_name,
    storageSystemListEntry_storageSystemArn,

    -- * TagListEntry
    TagListEntry (..),
    newTagListEntry,
    tagListEntry_value,
    tagListEntry_key,

    -- * TaskExecutionListEntry
    TaskExecutionListEntry (..),
    newTaskExecutionListEntry,
    taskExecutionListEntry_status,
    taskExecutionListEntry_taskExecutionArn,

    -- * TaskExecutionResultDetail
    TaskExecutionResultDetail (..),
    newTaskExecutionResultDetail,
    taskExecutionResultDetail_errorCode,
    taskExecutionResultDetail_errorDetail,
    taskExecutionResultDetail_prepareDuration,
    taskExecutionResultDetail_prepareStatus,
    taskExecutionResultDetail_totalDuration,
    taskExecutionResultDetail_transferDuration,
    taskExecutionResultDetail_transferStatus,
    taskExecutionResultDetail_verifyDuration,
    taskExecutionResultDetail_verifyStatus,

    -- * TaskFilter
    TaskFilter (..),
    newTaskFilter,
    taskFilter_name,
    taskFilter_values,
    taskFilter_operator,

    -- * TaskListEntry
    TaskListEntry (..),
    newTaskListEntry,
    taskListEntry_name,
    taskListEntry_status,
    taskListEntry_taskArn,

    -- * TaskSchedule
    TaskSchedule (..),
    newTaskSchedule,
    taskSchedule_scheduleExpression,

    -- * Throughput
    Throughput (..),
    newThroughput,
    throughput_other,
    throughput_read,
    throughput_total,
    throughput_write,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types.AgentListEntry
import Amazonka.DataSync.Types.AgentStatus
import Amazonka.DataSync.Types.Atime
import Amazonka.DataSync.Types.Capacity
import Amazonka.DataSync.Types.Credentials
import Amazonka.DataSync.Types.DiscoveryJobListEntry
import Amazonka.DataSync.Types.DiscoveryJobStatus
import Amazonka.DataSync.Types.DiscoveryResourceFilter
import Amazonka.DataSync.Types.DiscoveryResourceType
import Amazonka.DataSync.Types.DiscoveryServerConfiguration
import Amazonka.DataSync.Types.DiscoverySystemType
import Amazonka.DataSync.Types.Ec2Config
import Amazonka.DataSync.Types.EfsInTransitEncryption
import Amazonka.DataSync.Types.EndpointType
import Amazonka.DataSync.Types.FilterRule
import Amazonka.DataSync.Types.FilterType
import Amazonka.DataSync.Types.FsxProtocol
import Amazonka.DataSync.Types.FsxProtocolNfs
import Amazonka.DataSync.Types.FsxProtocolSmb
import Amazonka.DataSync.Types.Gid
import Amazonka.DataSync.Types.HdfsAuthenticationType
import Amazonka.DataSync.Types.HdfsDataTransferProtection
import Amazonka.DataSync.Types.HdfsNameNode
import Amazonka.DataSync.Types.HdfsRpcProtection
import Amazonka.DataSync.Types.IOPS
import Amazonka.DataSync.Types.Latency
import Amazonka.DataSync.Types.LocationFilter
import Amazonka.DataSync.Types.LocationFilterName
import Amazonka.DataSync.Types.LocationListEntry
import Amazonka.DataSync.Types.LogLevel
import Amazonka.DataSync.Types.MaxP95Performance
import Amazonka.DataSync.Types.Mtime
import Amazonka.DataSync.Types.NetAppONTAPCluster
import Amazonka.DataSync.Types.NetAppONTAPSVM
import Amazonka.DataSync.Types.NetAppONTAPVolume
import Amazonka.DataSync.Types.NfsMountOptions
import Amazonka.DataSync.Types.NfsVersion
import Amazonka.DataSync.Types.ObjectStorageServerProtocol
import Amazonka.DataSync.Types.ObjectTags
import Amazonka.DataSync.Types.OnPremConfig
import Amazonka.DataSync.Types.Operator
import Amazonka.DataSync.Types.Options
import Amazonka.DataSync.Types.OverwriteMode
import Amazonka.DataSync.Types.P95Metrics
import Amazonka.DataSync.Types.PhaseStatus
import Amazonka.DataSync.Types.PosixPermissions
import Amazonka.DataSync.Types.PreserveDeletedFiles
import Amazonka.DataSync.Types.PreserveDevices
import Amazonka.DataSync.Types.PrivateLinkConfig
import Amazonka.DataSync.Types.QopConfiguration
import Amazonka.DataSync.Types.Recommendation
import Amazonka.DataSync.Types.RecommendationStatus
import Amazonka.DataSync.Types.ResourceDetails
import Amazonka.DataSync.Types.ResourceMetrics
import Amazonka.DataSync.Types.S3Config
import Amazonka.DataSync.Types.S3StorageClass
import Amazonka.DataSync.Types.SmbMountOptions
import Amazonka.DataSync.Types.SmbSecurityDescriptorCopyFlags
import Amazonka.DataSync.Types.SmbVersion
import Amazonka.DataSync.Types.StorageSystemConnectivityStatus
import Amazonka.DataSync.Types.StorageSystemListEntry
import Amazonka.DataSync.Types.TagListEntry
import Amazonka.DataSync.Types.TaskExecutionListEntry
import Amazonka.DataSync.Types.TaskExecutionResultDetail
import Amazonka.DataSync.Types.TaskExecutionStatus
import Amazonka.DataSync.Types.TaskFilter
import Amazonka.DataSync.Types.TaskFilterName
import Amazonka.DataSync.Types.TaskListEntry
import Amazonka.DataSync.Types.TaskQueueing
import Amazonka.DataSync.Types.TaskSchedule
import Amazonka.DataSync.Types.TaskStatus
import Amazonka.DataSync.Types.Throughput
import Amazonka.DataSync.Types.TransferMode
import Amazonka.DataSync.Types.Uid
import Amazonka.DataSync.Types.VerifyMode
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-11-09@ of the Amazon DataSync SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DataSync",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "datasync",
      Core.signingName = "datasync",
      Core.version = "2018-11-09",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DataSync",
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

-- | This exception is thrown when an error occurs in the DataSync service.
_InternalException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"

-- | This exception is thrown when the client submits a malformed request.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
