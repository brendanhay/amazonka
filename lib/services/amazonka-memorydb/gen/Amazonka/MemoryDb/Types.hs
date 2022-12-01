{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MemoryDb.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NodeQuotaForClusterExceededFault,
    _ClusterQuotaForCustomerExceededFault,
    _SubnetQuotaExceededFault,
    _SnapshotQuotaExceededFault,
    _InvalidSubnet,
    _ServiceUpdateNotFoundFault,
    _UserNotFoundFault,
    _ACLNotFoundFault,
    _ShardNotFoundFault,
    _ClusterNotFoundFault,
    _InvalidSnapshotStateFault,
    _NoOperationFault,
    _UserAlreadyExistsFault,
    _TagNotFoundFault,
    _SubnetGroupInUseFault,
    _ClusterAlreadyExistsFault,
    _ACLAlreadyExistsFault,
    _DefaultUserRequired,
    _InvalidParameterCombinationException,
    _ParameterGroupAlreadyExistsFault,
    _ServiceLinkedRoleNotFoundFault,
    _ShardsPerClusterQuotaExceededFault,
    _InvalidParameterGroupStateFault,
    _UserQuotaExceededFault,
    _DuplicateUserNameFault,
    _ParameterGroupNotFoundFault,
    _InvalidACLStateFault,
    _TagQuotaPerResourceExceeded,
    _InvalidARNFault,
    _SnapshotAlreadyExistsFault,
    _InvalidNodeStateFault,
    _TestFailoverNotAvailableFault,
    _NodeQuotaForCustomerExceededFault,
    _SubnetGroupNotFoundFault,
    _InvalidVPCNetworkStateFault,
    _SubnetInUse,
    _InvalidUserStateFault,
    _SnapshotNotFoundFault,
    _InvalidClusterStateFault,
    _InvalidKMSKeyFault,
    _SubnetGroupAlreadyExistsFault,
    _ParameterGroupQuotaExceededFault,
    _InsufficientClusterCapacityFault,
    _ACLQuotaExceededFault,
    _InvalidParameterValueException,
    _InvalidCredentialsException,
    _SubnetNotAllowedFault,
    _APICallRateForCustomerExceededFault,
    _SubnetGroupQuotaExceededFault,

    -- * AZStatus
    AZStatus (..),

    -- * AuthenticationType
    AuthenticationType (..),

    -- * DataTieringStatus
    DataTieringStatus (..),

    -- * InputAuthenticationType
    InputAuthenticationType (..),

    -- * ServiceUpdateStatus
    ServiceUpdateStatus (..),

    -- * ServiceUpdateType
    ServiceUpdateType (..),

    -- * SourceType
    SourceType (..),

    -- * ACL
    ACL (..),
    newACL,
    acl_name,
    acl_arn,
    acl_pendingChanges,
    acl_clusters,
    acl_userNames,
    acl_status,
    acl_minimumEngineVersion,

    -- * ACLPendingChanges
    ACLPendingChanges (..),
    newACLPendingChanges,
    aCLPendingChanges_userNamesToRemove,
    aCLPendingChanges_userNamesToAdd,

    -- * ACLsUpdateStatus
    ACLsUpdateStatus (..),
    newACLsUpdateStatus,
    aCLsUpdateStatus_aCLToApply,

    -- * Authentication
    Authentication (..),
    newAuthentication,
    authentication_type,
    authentication_passwordCount,

    -- * AuthenticationMode
    AuthenticationMode (..),
    newAuthenticationMode,
    authenticationMode_passwords,
    authenticationMode_type,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * Cluster
    Cluster (..),
    newCluster,
    cluster_subnetGroupName,
    cluster_parameterGroupName,
    cluster_name,
    cluster_aCLName,
    cluster_autoMinorVersionUpgrade,
    cluster_enginePatchVersion,
    cluster_snsTopicStatus,
    cluster_arn,
    cluster_tLSEnabled,
    cluster_clusterEndpoint,
    cluster_availabilityMode,
    cluster_status,
    cluster_description,
    cluster_nodeType,
    cluster_maintenanceWindow,
    cluster_snapshotWindow,
    cluster_numberOfShards,
    cluster_snapshotRetentionLimit,
    cluster_snsTopicArn,
    cluster_pendingUpdates,
    cluster_securityGroups,
    cluster_shards,
    cluster_kmsKeyId,
    cluster_parameterGroupStatus,
    cluster_dataTiering,
    cluster_engineVersion,

    -- * ClusterConfiguration
    ClusterConfiguration (..),
    newClusterConfiguration,
    clusterConfiguration_port,
    clusterConfiguration_subnetGroupName,
    clusterConfiguration_parameterGroupName,
    clusterConfiguration_name,
    clusterConfiguration_topicArn,
    clusterConfiguration_description,
    clusterConfiguration_nodeType,
    clusterConfiguration_maintenanceWindow,
    clusterConfiguration_snapshotWindow,
    clusterConfiguration_snapshotRetentionLimit,
    clusterConfiguration_shards,
    clusterConfiguration_numShards,
    clusterConfiguration_vpcId,
    clusterConfiguration_engineVersion,

    -- * ClusterPendingUpdates
    ClusterPendingUpdates (..),
    newClusterPendingUpdates,
    clusterPendingUpdates_resharding,
    clusterPendingUpdates_aCLs,
    clusterPendingUpdates_serviceUpdates,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_port,
    endpoint_address,

    -- * EngineVersionInfo
    EngineVersionInfo (..),
    newEngineVersionInfo,
    engineVersionInfo_parameterGroupFamily,
    engineVersionInfo_enginePatchVersion,
    engineVersionInfo_engineVersion,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_sourceName,
    event_date,
    event_sourceType,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * Node
    Node (..),
    newNode,
    node_name,
    node_status,
    node_availabilityZone,
    node_createTime,
    node_endpoint,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_name,
    parameter_description,
    parameter_minimumEngineVersion,
    parameter_allowedValues,
    parameter_dataType,
    parameter_value,

    -- * ParameterGroup
    ParameterGroup (..),
    newParameterGroup,
    parameterGroup_name,
    parameterGroup_arn,
    parameterGroup_description,
    parameterGroup_family,

    -- * ParameterNameValue
    ParameterNameValue (..),
    newParameterNameValue,
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- * PendingModifiedServiceUpdate
    PendingModifiedServiceUpdate (..),
    newPendingModifiedServiceUpdate,
    pendingModifiedServiceUpdate_status,
    pendingModifiedServiceUpdate_serviceUpdateName,

    -- * ReplicaConfigurationRequest
    ReplicaConfigurationRequest (..),
    newReplicaConfigurationRequest,
    replicaConfigurationRequest_replicaCount,

    -- * ReshardingStatus
    ReshardingStatus (..),
    newReshardingStatus,
    reshardingStatus_slotMigration,

    -- * SecurityGroupMembership
    SecurityGroupMembership (..),
    newSecurityGroupMembership,
    securityGroupMembership_securityGroupId,
    securityGroupMembership_status,

    -- * ServiceUpdate
    ServiceUpdate (..),
    newServiceUpdate,
    serviceUpdate_type,
    serviceUpdate_releaseDate,
    serviceUpdate_status,
    serviceUpdate_description,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_autoUpdateStartDate,
    serviceUpdate_clusterName,
    serviceUpdate_nodesUpdated,

    -- * ServiceUpdateRequest
    ServiceUpdateRequest (..),
    newServiceUpdateRequest,
    serviceUpdateRequest_serviceUpdateNameToApply,

    -- * Shard
    Shard (..),
    newShard,
    shard_name,
    shard_nodes,
    shard_status,
    shard_numberOfNodes,
    shard_slots,

    -- * ShardConfiguration
    ShardConfiguration (..),
    newShardConfiguration,
    shardConfiguration_slots,
    shardConfiguration_replicaCount,

    -- * ShardConfigurationRequest
    ShardConfigurationRequest (..),
    newShardConfigurationRequest,
    shardConfigurationRequest_shardCount,

    -- * ShardDetail
    ShardDetail (..),
    newShardDetail,
    shardDetail_name,
    shardDetail_configuration,
    shardDetail_snapshotCreationTime,
    shardDetail_size,

    -- * SlotMigration
    SlotMigration (..),
    newSlotMigration,
    slotMigration_progressPercentage,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_clusterConfiguration,
    snapshot_name,
    snapshot_arn,
    snapshot_status,
    snapshot_source,
    snapshot_kmsKeyId,
    snapshot_dataTiering,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_availabilityZone,
    subnet_identifier,

    -- * SubnetGroup
    SubnetGroup (..),
    newSubnetGroup,
    subnetGroup_name,
    subnetGroup_subnets,
    subnetGroup_arn,
    subnetGroup_description,
    subnetGroup_vpcId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UnprocessedCluster
    UnprocessedCluster (..),
    newUnprocessedCluster,
    unprocessedCluster_errorMessage,
    unprocessedCluster_clusterName,
    unprocessedCluster_errorType,

    -- * User
    User (..),
    newUser,
    user_accessString,
    user_authentication,
    user_name,
    user_arn,
    user_status,
    user_minimumEngineVersion,
    user_aCLNames,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types.ACL
import Amazonka.MemoryDb.Types.ACLPendingChanges
import Amazonka.MemoryDb.Types.ACLsUpdateStatus
import Amazonka.MemoryDb.Types.AZStatus
import Amazonka.MemoryDb.Types.Authentication
import Amazonka.MemoryDb.Types.AuthenticationMode
import Amazonka.MemoryDb.Types.AuthenticationType
import Amazonka.MemoryDb.Types.AvailabilityZone
import Amazonka.MemoryDb.Types.Cluster
import Amazonka.MemoryDb.Types.ClusterConfiguration
import Amazonka.MemoryDb.Types.ClusterPendingUpdates
import Amazonka.MemoryDb.Types.DataTieringStatus
import Amazonka.MemoryDb.Types.Endpoint
import Amazonka.MemoryDb.Types.EngineVersionInfo
import Amazonka.MemoryDb.Types.Event
import Amazonka.MemoryDb.Types.Filter
import Amazonka.MemoryDb.Types.InputAuthenticationType
import Amazonka.MemoryDb.Types.Node
import Amazonka.MemoryDb.Types.Parameter
import Amazonka.MemoryDb.Types.ParameterGroup
import Amazonka.MemoryDb.Types.ParameterNameValue
import Amazonka.MemoryDb.Types.PendingModifiedServiceUpdate
import Amazonka.MemoryDb.Types.ReplicaConfigurationRequest
import Amazonka.MemoryDb.Types.ReshardingStatus
import Amazonka.MemoryDb.Types.SecurityGroupMembership
import Amazonka.MemoryDb.Types.ServiceUpdate
import Amazonka.MemoryDb.Types.ServiceUpdateRequest
import Amazonka.MemoryDb.Types.ServiceUpdateStatus
import Amazonka.MemoryDb.Types.ServiceUpdateType
import Amazonka.MemoryDb.Types.Shard
import Amazonka.MemoryDb.Types.ShardConfiguration
import Amazonka.MemoryDb.Types.ShardConfigurationRequest
import Amazonka.MemoryDb.Types.ShardDetail
import Amazonka.MemoryDb.Types.SlotMigration
import Amazonka.MemoryDb.Types.Snapshot
import Amazonka.MemoryDb.Types.SourceType
import Amazonka.MemoryDb.Types.Subnet
import Amazonka.MemoryDb.Types.SubnetGroup
import Amazonka.MemoryDb.Types.Tag
import Amazonka.MemoryDb.Types.UnprocessedCluster
import Amazonka.MemoryDb.Types.User
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2021-01-01@ of the Amazon MemoryDB SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MemoryDb",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "memory-db",
      Core.signingName = "memorydb",
      Core.version = "2021-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MemoryDb",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- |
_NodeQuotaForClusterExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceededFault"

-- |
_ClusterQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceededFault"

-- |
_SubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetQuotaExceededFault"

-- |
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceededFault"

-- |
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

-- |
_ServiceUpdateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUpdateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceUpdateNotFoundFault"

-- |
_UserNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserNotFoundFault"

-- |
_ACLNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ACLNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ACLNotFoundFault"

-- |
_ShardNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ShardNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ShardNotFoundFault"

-- |
_ClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterNotFoundFault"

-- |
_InvalidSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotStateFault"

-- |
_NoOperationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoOperationFault =
  Core._MatchServiceError
    defaultService
    "NoOperationFault"

-- |
_UserAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserAlreadyExistsFault"

-- |
_TagNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFoundFault"

-- |
_SubnetGroupInUseFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupInUseFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupInUseFault"

-- |
_ClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterAlreadyExistsFault"

-- |
_ACLAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ACLAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ACLAlreadyExistsFault"

-- |
_DefaultUserRequired :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultUserRequired =
  Core._MatchServiceError
    defaultService
    "DefaultUserRequired"

-- |
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

-- |
_ParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupAlreadyExistsFault"

-- |
_ServiceLinkedRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"

-- |
_ShardsPerClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ShardsPerClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ShardsPerClusterQuotaExceededFault"

-- |
_InvalidParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidParameterGroupStateFault"

-- |
_UserQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserQuotaExceededFault"

-- |
_DuplicateUserNameFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateUserNameFault =
  Core._MatchServiceError
    defaultService
    "DuplicateUserNameFault"

-- |
_ParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupNotFoundFault"

-- |
_InvalidACLStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidACLStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidACLStateFault"

-- |
_TagQuotaPerResourceExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"

-- |
_InvalidARNFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError
    defaultService
    "InvalidARNFault"

-- |
_SnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotAlreadyExistsFault"

-- |
_InvalidNodeStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNodeStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidNodeStateFault"

-- |
_TestFailoverNotAvailableFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TestFailoverNotAvailableFault =
  Core._MatchServiceError
    defaultService
    "TestFailoverNotAvailableFault"

-- |
_NodeQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceededFault"

-- |
_SubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupNotFoundFault"

-- |
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"

-- |
_SubnetInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError
    defaultService
    "SubnetInUse"

-- |
_InvalidUserStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidUserStateFault"

-- |
_SnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotNotFoundFault"

-- |
_InvalidClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterStateFault"

-- |
_InvalidKMSKeyFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKMSKeyFault =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyFault"

-- |
_SubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupAlreadyExistsFault"

-- |
_ParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupQuotaExceededFault"

-- |
_InsufficientClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientClusterCapacityFault"

-- |
_ACLQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ACLQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ACLQuotaExceededFault"

-- |
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- |
_InvalidCredentialsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCredentialsException =
  Core._MatchServiceError
    defaultService
    "InvalidCredentialsException"

-- |
_SubnetNotAllowedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "SubnetNotAllowedFault"

-- |
_APICallRateForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_APICallRateForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "APICallRateForCustomerExceededFault"

-- |
_SubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupQuotaExceededFault"
