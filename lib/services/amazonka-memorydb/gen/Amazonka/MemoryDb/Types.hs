{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MemoryDb.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MemoryDb.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ACLAlreadyExistsFault,
    _ACLNotFoundFault,
    _ACLQuotaExceededFault,
    _APICallRateForCustomerExceededFault,
    _ClusterAlreadyExistsFault,
    _ClusterNotFoundFault,
    _ClusterQuotaForCustomerExceededFault,
    _DefaultUserRequired,
    _DuplicateUserNameFault,
    _InsufficientClusterCapacityFault,
    _InvalidACLStateFault,
    _InvalidARNFault,
    _InvalidClusterStateFault,
    _InvalidCredentialsException,
    _InvalidKMSKeyFault,
    _InvalidNodeStateFault,
    _InvalidParameterCombinationException,
    _InvalidParameterGroupStateFault,
    _InvalidParameterValueException,
    _InvalidSnapshotStateFault,
    _InvalidSubnet,
    _InvalidUserStateFault,
    _InvalidVPCNetworkStateFault,
    _NoOperationFault,
    _NodeQuotaForClusterExceededFault,
    _NodeQuotaForCustomerExceededFault,
    _ParameterGroupAlreadyExistsFault,
    _ParameterGroupNotFoundFault,
    _ParameterGroupQuotaExceededFault,
    _ReservedNodeAlreadyExistsFault,
    _ReservedNodeNotFoundFault,
    _ReservedNodeQuotaExceededFault,
    _ReservedNodesOfferingNotFoundFault,
    _ServiceLinkedRoleNotFoundFault,
    _ServiceUpdateNotFoundFault,
    _ShardNotFoundFault,
    _ShardsPerClusterQuotaExceededFault,
    _SnapshotAlreadyExistsFault,
    _SnapshotNotFoundFault,
    _SnapshotQuotaExceededFault,
    _SubnetGroupAlreadyExistsFault,
    _SubnetGroupInUseFault,
    _SubnetGroupNotFoundFault,
    _SubnetGroupQuotaExceededFault,
    _SubnetInUse,
    _SubnetNotAllowedFault,
    _SubnetQuotaExceededFault,
    _TagNotFoundFault,
    _TagQuotaPerResourceExceeded,
    _TestFailoverNotAvailableFault,
    _UserAlreadyExistsFault,
    _UserNotFoundFault,
    _UserQuotaExceededFault,

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
    acl_arn,
    acl_clusters,
    acl_minimumEngineVersion,
    acl_name,
    acl_pendingChanges,
    acl_status,
    acl_userNames,

    -- * ACLPendingChanges
    ACLPendingChanges (..),
    newACLPendingChanges,
    aCLPendingChanges_userNamesToAdd,
    aCLPendingChanges_userNamesToRemove,

    -- * ACLsUpdateStatus
    ACLsUpdateStatus (..),
    newACLsUpdateStatus,
    aCLsUpdateStatus_aCLToApply,

    -- * Authentication
    Authentication (..),
    newAuthentication,
    authentication_passwordCount,
    authentication_type,

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
    cluster_aCLName,
    cluster_arn,
    cluster_autoMinorVersionUpgrade,
    cluster_availabilityMode,
    cluster_clusterEndpoint,
    cluster_dataTiering,
    cluster_description,
    cluster_enginePatchVersion,
    cluster_engineVersion,
    cluster_kmsKeyId,
    cluster_maintenanceWindow,
    cluster_name,
    cluster_nodeType,
    cluster_numberOfShards,
    cluster_parameterGroupName,
    cluster_parameterGroupStatus,
    cluster_pendingUpdates,
    cluster_securityGroups,
    cluster_shards,
    cluster_snapshotRetentionLimit,
    cluster_snapshotWindow,
    cluster_snsTopicArn,
    cluster_snsTopicStatus,
    cluster_status,
    cluster_subnetGroupName,
    cluster_tLSEnabled,

    -- * ClusterConfiguration
    ClusterConfiguration (..),
    newClusterConfiguration,
    clusterConfiguration_description,
    clusterConfiguration_engineVersion,
    clusterConfiguration_maintenanceWindow,
    clusterConfiguration_name,
    clusterConfiguration_nodeType,
    clusterConfiguration_numShards,
    clusterConfiguration_parameterGroupName,
    clusterConfiguration_port,
    clusterConfiguration_shards,
    clusterConfiguration_snapshotRetentionLimit,
    clusterConfiguration_snapshotWindow,
    clusterConfiguration_subnetGroupName,
    clusterConfiguration_topicArn,
    clusterConfiguration_vpcId,

    -- * ClusterPendingUpdates
    ClusterPendingUpdates (..),
    newClusterPendingUpdates,
    clusterPendingUpdates_aCLs,
    clusterPendingUpdates_resharding,
    clusterPendingUpdates_serviceUpdates,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,

    -- * EngineVersionInfo
    EngineVersionInfo (..),
    newEngineVersionInfo,
    engineVersionInfo_enginePatchVersion,
    engineVersionInfo_engineVersion,
    engineVersionInfo_parameterGroupFamily,

    -- * Event
    Event (..),
    newEvent,
    event_date,
    event_message,
    event_sourceName,
    event_sourceType,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * Node
    Node (..),
    newNode,
    node_availabilityZone,
    node_createTime,
    node_endpoint,
    node_name,
    node_status,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_allowedValues,
    parameter_dataType,
    parameter_description,
    parameter_minimumEngineVersion,
    parameter_name,
    parameter_value,

    -- * ParameterGroup
    ParameterGroup (..),
    newParameterGroup,
    parameterGroup_arn,
    parameterGroup_description,
    parameterGroup_family,
    parameterGroup_name,

    -- * ParameterNameValue
    ParameterNameValue (..),
    newParameterNameValue,
    parameterNameValue_parameterName,
    parameterNameValue_parameterValue,

    -- * PendingModifiedServiceUpdate
    PendingModifiedServiceUpdate (..),
    newPendingModifiedServiceUpdate,
    pendingModifiedServiceUpdate_serviceUpdateName,
    pendingModifiedServiceUpdate_status,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- * ReplicaConfigurationRequest
    ReplicaConfigurationRequest (..),
    newReplicaConfigurationRequest,
    replicaConfigurationRequest_replicaCount,

    -- * ReservedNode
    ReservedNode (..),
    newReservedNode,
    reservedNode_arn,
    reservedNode_duration,
    reservedNode_fixedPrice,
    reservedNode_nodeCount,
    reservedNode_nodeType,
    reservedNode_offeringType,
    reservedNode_recurringCharges,
    reservedNode_reservationId,
    reservedNode_reservedNodesOfferingId,
    reservedNode_startTime,
    reservedNode_state,

    -- * ReservedNodesOffering
    ReservedNodesOffering (..),
    newReservedNodesOffering,
    reservedNodesOffering_duration,
    reservedNodesOffering_fixedPrice,
    reservedNodesOffering_nodeType,
    reservedNodesOffering_offeringType,
    reservedNodesOffering_recurringCharges,
    reservedNodesOffering_reservedNodesOfferingId,

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
    serviceUpdate_autoUpdateStartDate,
    serviceUpdate_clusterName,
    serviceUpdate_description,
    serviceUpdate_nodesUpdated,
    serviceUpdate_releaseDate,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_status,
    serviceUpdate_type,

    -- * ServiceUpdateRequest
    ServiceUpdateRequest (..),
    newServiceUpdateRequest,
    serviceUpdateRequest_serviceUpdateNameToApply,

    -- * Shard
    Shard (..),
    newShard,
    shard_name,
    shard_nodes,
    shard_numberOfNodes,
    shard_slots,
    shard_status,

    -- * ShardConfiguration
    ShardConfiguration (..),
    newShardConfiguration,
    shardConfiguration_replicaCount,
    shardConfiguration_slots,

    -- * ShardConfigurationRequest
    ShardConfigurationRequest (..),
    newShardConfigurationRequest,
    shardConfigurationRequest_shardCount,

    -- * ShardDetail
    ShardDetail (..),
    newShardDetail,
    shardDetail_configuration,
    shardDetail_name,
    shardDetail_size,
    shardDetail_snapshotCreationTime,

    -- * SlotMigration
    SlotMigration (..),
    newSlotMigration,
    slotMigration_progressPercentage,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_arn,
    snapshot_clusterConfiguration,
    snapshot_dataTiering,
    snapshot_kmsKeyId,
    snapshot_name,
    snapshot_source,
    snapshot_status,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_availabilityZone,
    subnet_identifier,

    -- * SubnetGroup
    SubnetGroup (..),
    newSubnetGroup,
    subnetGroup_arn,
    subnetGroup_description,
    subnetGroup_name,
    subnetGroup_subnets,
    subnetGroup_vpcId,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UnprocessedCluster
    UnprocessedCluster (..),
    newUnprocessedCluster,
    unprocessedCluster_clusterName,
    unprocessedCluster_errorMessage,
    unprocessedCluster_errorType,

    -- * User
    User (..),
    newUser,
    user_aCLNames,
    user_arn,
    user_accessString,
    user_authentication,
    user_minimumEngineVersion,
    user_name,
    user_status,
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
import Amazonka.MemoryDb.Types.RecurringCharge
import Amazonka.MemoryDb.Types.ReplicaConfigurationRequest
import Amazonka.MemoryDb.Types.ReservedNode
import Amazonka.MemoryDb.Types.ReservedNodesOffering
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

_ACLAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ACLAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ACLAlreadyExistsFault"

_ACLNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ACLNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ACLNotFoundFault"

_ACLQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ACLQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ACLQuotaExceededFault"

_APICallRateForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_APICallRateForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "APICallRateForCustomerExceededFault"

_ClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterAlreadyExistsFault"

_ClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterNotFoundFault"

_ClusterQuotaForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceededFault"

_DefaultUserRequired :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DefaultUserRequired =
  Core._MatchServiceError
    defaultService
    "DefaultUserRequired"

_DuplicateUserNameFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateUserNameFault =
  Core._MatchServiceError
    defaultService
    "DuplicateUserNameFault"

_InsufficientClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientClusterCapacityFault"

_InvalidACLStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidACLStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidACLStateFault"

_InvalidARNFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError
    defaultService
    "InvalidARNFault"

_InvalidClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterStateFault"

_InvalidCredentialsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCredentialsException =
  Core._MatchServiceError
    defaultService
    "InvalidCredentialsException"

_InvalidKMSKeyFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidKMSKeyFault =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyFault"

_InvalidNodeStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidNodeStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidNodeStateFault"

_InvalidParameterCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"

_InvalidParameterGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidParameterGroupStateFault"

_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

_InvalidSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotStateFault"

_InvalidSubnet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

_InvalidUserStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidUserStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidUserStateFault"

_InvalidVPCNetworkStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"

_NoOperationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoOperationFault =
  Core._MatchServiceError
    defaultService
    "NoOperationFault"

_NodeQuotaForClusterExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceededFault"

_NodeQuotaForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceededFault"

_ParameterGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupAlreadyExistsFault"

_ParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupNotFoundFault"

_ParameterGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupQuotaExceededFault"

-- | You already have a reservation with the given identifier.
_ReservedNodeAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeAlreadyExistsFault"

-- | The requested node does not exist.
_ReservedNodeNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeNotFoundFault"

-- | The request cannot be processed because it would exceed the user\'s node
-- quota.
_ReservedNodeQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodeQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodeQuotaExceededFault"

-- | The requested node offering does not exist.
_ReservedNodesOfferingNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedNodesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedNodesOfferingNotFoundFault"

_ServiceLinkedRoleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"

_ServiceUpdateNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUpdateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceUpdateNotFoundFault"

_ShardNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ShardNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ShardNotFoundFault"

_ShardsPerClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ShardsPerClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ShardsPerClusterQuotaExceededFault"

_SnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotAlreadyExistsFault"

_SnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotNotFoundFault"

_SnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceededFault"

_SubnetGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupAlreadyExistsFault"

_SubnetGroupInUseFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupInUseFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupInUseFault"

_SubnetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupNotFoundFault"

_SubnetGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupQuotaExceededFault"

_SubnetInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError
    defaultService
    "SubnetInUse"

_SubnetNotAllowedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "SubnetNotAllowedFault"

_SubnetQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetQuotaExceededFault"

_TagNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFoundFault"

_TagQuotaPerResourceExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"

_TestFailoverNotAvailableFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TestFailoverNotAvailableFault =
  Core._MatchServiceError
    defaultService
    "TestFailoverNotAvailableFault"

_UserAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserAlreadyExistsFault"

_UserNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserNotFoundFault"

_UserQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserQuotaExceededFault"
