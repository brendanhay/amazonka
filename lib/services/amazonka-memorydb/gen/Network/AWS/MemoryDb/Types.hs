{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MemoryDb.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MemoryDb.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidNodeStateFault,
    _SubnetQuotaExceededFault,
    _ACLNotFoundFault,
    _ParameterGroupNotFoundFault,
    _InvalidParameterGroupStateFault,
    _SubnetGroupInUseFault,
    _ParameterGroupAlreadyExistsFault,
    _InvalidSubnet,
    _TagQuotaPerResourceExceeded,
    _ACLAlreadyExistsFault,
    _ShardNotFoundFault,
    _UserAlreadyExistsFault,
    _InvalidUserStateFault,
    _SnapshotNotFoundFault,
    _InvalidSnapshotStateFault,
    _SnapshotAlreadyExistsFault,
    _DefaultUserRequired,
    _ClusterNotFoundFault,
    _TagNotFoundFault,
    _SnapshotQuotaExceededFault,
    _NodeQuotaForClusterExceededFault,
    _APICallRateForCustomerExceededFault,
    _InvalidClusterStateFault,
    _ACLQuotaExceededFault,
    _ServiceLinkedRoleNotFoundFault,
    _InvalidKMSKeyFault,
    _InsufficientClusterCapacityFault,
    _ParameterGroupQuotaExceededFault,
    _InvalidParameterValueException,
    _TestFailoverNotAvailableFault,
    _SubnetNotAllowedFault,
    _InvalidVPCNetworkStateFault,
    _SubnetInUse,
    _ClusterQuotaForCustomerExceededFault,
    _InvalidACLStateFault,
    _ShardsPerClusterQuotaExceededFault,
    _ServiceUpdateNotFoundFault,
    _SubnetGroupNotFoundFault,
    _SubnetGroupAlreadyExistsFault,
    _UserNotFoundFault,
    _NodeQuotaForCustomerExceededFault,
    _SubnetGroupQuotaExceededFault,
    _DuplicateUserNameFault,
    _ClusterAlreadyExistsFault,
    _UserQuotaExceededFault,
    _InvalidARNFault,
    _NoOperationFault,
    _InvalidCredentialsException,
    _InvalidParameterCombinationException,

    -- * AZStatus
    AZStatus (..),

    -- * AuthenticationType
    AuthenticationType (..),

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
    acl_status,
    acl_userNames,
    acl_arn,
    acl_name,
    acl_pendingChanges,
    acl_minimumEngineVersion,
    acl_clusters,

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
    cluster_engineVersion,
    cluster_status,
    cluster_autoMinorVersionUpgrade,
    cluster_snsTopicArn,
    cluster_securityGroups,
    cluster_availabilityMode,
    cluster_parameterGroupStatus,
    cluster_arn,
    cluster_pendingUpdates,
    cluster_numberOfShards,
    cluster_snapshotWindow,
    cluster_subnetGroupName,
    cluster_tLSEnabled,
    cluster_maintenanceWindow,
    cluster_kmsKeyId,
    cluster_shards,
    cluster_name,
    cluster_enginePatchVersion,
    cluster_snapshotRetentionLimit,
    cluster_nodeType,
    cluster_snsTopicStatus,
    cluster_description,
    cluster_aCLName,
    cluster_clusterEndpoint,
    cluster_parameterGroupName,

    -- * ClusterConfiguration
    ClusterConfiguration (..),
    newClusterConfiguration,
    clusterConfiguration_engineVersion,
    clusterConfiguration_vpcId,
    clusterConfiguration_snapshotWindow,
    clusterConfiguration_subnetGroupName,
    clusterConfiguration_numShards,
    clusterConfiguration_maintenanceWindow,
    clusterConfiguration_topicArn,
    clusterConfiguration_shards,
    clusterConfiguration_name,
    clusterConfiguration_snapshotRetentionLimit,
    clusterConfiguration_nodeType,
    clusterConfiguration_description,
    clusterConfiguration_port,
    clusterConfiguration_parameterGroupName,

    -- * ClusterPendingUpdates
    ClusterPendingUpdates (..),
    newClusterPendingUpdates,
    clusterPendingUpdates_serviceUpdates,
    clusterPendingUpdates_resharding,
    clusterPendingUpdates_aCLs,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,

    -- * EngineVersionInfo
    EngineVersionInfo (..),
    newEngineVersionInfo,
    engineVersionInfo_engineVersion,
    engineVersionInfo_enginePatchVersion,
    engineVersionInfo_parameterGroupFamily,

    -- * Event
    Event (..),
    newEvent,
    event_sourceName,
    event_sourceType,
    event_date,
    event_message,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * Node
    Node (..),
    newNode,
    node_status,
    node_availabilityZone,
    node_name,
    node_endpoint,
    node_createTime,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_value,
    parameter_name,
    parameter_minimumEngineVersion,
    parameter_dataType,
    parameter_allowedValues,
    parameter_description,

    -- * ParameterGroup
    ParameterGroup (..),
    newParameterGroup,
    parameterGroup_arn,
    parameterGroup_family,
    parameterGroup_name,
    parameterGroup_description,

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
    securityGroupMembership_status,
    securityGroupMembership_securityGroupId,

    -- * ServiceUpdate
    ServiceUpdate (..),
    newServiceUpdate,
    serviceUpdate_status,
    serviceUpdate_autoUpdateStartDate,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_nodesUpdated,
    serviceUpdate_releaseDate,
    serviceUpdate_clusterName,
    serviceUpdate_type,
    serviceUpdate_description,

    -- * ServiceUpdateRequest
    ServiceUpdateRequest (..),
    newServiceUpdateRequest,
    serviceUpdateRequest_serviceUpdateNameToApply,

    -- * Shard
    Shard (..),
    newShard,
    shard_status,
    shard_slots,
    shard_numberOfNodes,
    shard_name,
    shard_nodes,

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
    shardDetail_size,
    shardDetail_snapshotCreationTime,
    shardDetail_name,
    shardDetail_configuration,

    -- * SlotMigration
    SlotMigration (..),
    newSlotMigration,
    slotMigration_progressPercentage,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_status,
    snapshot_arn,
    snapshot_kmsKeyId,
    snapshot_name,
    snapshot_clusterConfiguration,
    snapshot_source,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_identifier,
    subnet_availabilityZone,

    -- * SubnetGroup
    SubnetGroup (..),
    newSubnetGroup,
    subnetGroup_arn,
    subnetGroup_vpcId,
    subnetGroup_subnets,
    subnetGroup_name,
    subnetGroup_description,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * UnprocessedCluster
    UnprocessedCluster (..),
    newUnprocessedCluster,
    unprocessedCluster_clusterName,
    unprocessedCluster_errorType,
    unprocessedCluster_errorMessage,

    -- * User
    User (..),
    newUser,
    user_status,
    user_arn,
    user_authentication,
    user_accessString,
    user_name,
    user_aCLNames,
    user_minimumEngineVersion,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MemoryDb.Types.ACL
import Network.AWS.MemoryDb.Types.ACLPendingChanges
import Network.AWS.MemoryDb.Types.ACLsUpdateStatus
import Network.AWS.MemoryDb.Types.AZStatus
import Network.AWS.MemoryDb.Types.Authentication
import Network.AWS.MemoryDb.Types.AuthenticationMode
import Network.AWS.MemoryDb.Types.AuthenticationType
import Network.AWS.MemoryDb.Types.AvailabilityZone
import Network.AWS.MemoryDb.Types.Cluster
import Network.AWS.MemoryDb.Types.ClusterConfiguration
import Network.AWS.MemoryDb.Types.ClusterPendingUpdates
import Network.AWS.MemoryDb.Types.Endpoint
import Network.AWS.MemoryDb.Types.EngineVersionInfo
import Network.AWS.MemoryDb.Types.Event
import Network.AWS.MemoryDb.Types.Filter
import Network.AWS.MemoryDb.Types.InputAuthenticationType
import Network.AWS.MemoryDb.Types.Node
import Network.AWS.MemoryDb.Types.Parameter
import Network.AWS.MemoryDb.Types.ParameterGroup
import Network.AWS.MemoryDb.Types.ParameterNameValue
import Network.AWS.MemoryDb.Types.PendingModifiedServiceUpdate
import Network.AWS.MemoryDb.Types.ReplicaConfigurationRequest
import Network.AWS.MemoryDb.Types.ReshardingStatus
import Network.AWS.MemoryDb.Types.SecurityGroupMembership
import Network.AWS.MemoryDb.Types.ServiceUpdate
import Network.AWS.MemoryDb.Types.ServiceUpdateRequest
import Network.AWS.MemoryDb.Types.ServiceUpdateStatus
import Network.AWS.MemoryDb.Types.ServiceUpdateType
import Network.AWS.MemoryDb.Types.Shard
import Network.AWS.MemoryDb.Types.ShardConfiguration
import Network.AWS.MemoryDb.Types.ShardConfigurationRequest
import Network.AWS.MemoryDb.Types.ShardDetail
import Network.AWS.MemoryDb.Types.SlotMigration
import Network.AWS.MemoryDb.Types.Snapshot
import Network.AWS.MemoryDb.Types.SourceType
import Network.AWS.MemoryDb.Types.Subnet
import Network.AWS.MemoryDb.Types.SubnetGroup
import Network.AWS.MemoryDb.Types.Tag
import Network.AWS.MemoryDb.Types.UnprocessedCluster
import Network.AWS.MemoryDb.Types.User
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2021-01-01@ of the Amazon MemoryDB SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MemoryDb",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "memory-db",
      Core._serviceSigningName = "memorydb",
      Core._serviceVersion = "2021-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "MemoryDb",
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

-- |
_InvalidNodeStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidNodeStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidNodeStateFault"

-- |
_SubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetQuotaExceededFault"

-- |
_ACLNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ACLNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ACLNotFoundFault"

-- |
_ParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupNotFoundFault"

-- |
_InvalidParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidParameterGroupStateFault"

-- |
_SubnetGroupInUseFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupInUseFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupInUseFault"

-- |
_ParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupAlreadyExistsFault"

-- |
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"

-- |
_TagQuotaPerResourceExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"

-- |
_ACLAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ACLAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ACLAlreadyExistsFault"

-- |
_ShardNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ShardNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ShardNotFoundFault"

-- |
_UserAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserAlreadyExistsFault"

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
_InvalidSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotStateFault"

-- |
_SnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotAlreadyExistsFault"

-- |
_DefaultUserRequired :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultUserRequired =
  Core._MatchServiceError
    defaultService
    "DefaultUserRequired"

-- |
_ClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ClusterNotFoundFault"

-- |
_TagNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFoundFault"

-- |
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceededFault"

-- |
_NodeQuotaForClusterExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceededFault"

-- |
_APICallRateForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_APICallRateForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "APICallRateForCustomerExceededFault"

-- |
_InvalidClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidClusterStateFault"

-- |
_ACLQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ACLQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ACLQuotaExceededFault"

-- |
_ServiceLinkedRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"

-- |
_InvalidKMSKeyFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKMSKeyFault =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyFault"

-- |
_InsufficientClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientClusterCapacityFault"

-- |
_ParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ParameterGroupQuotaExceededFault"

-- |
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- |
_TestFailoverNotAvailableFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TestFailoverNotAvailableFault =
  Core._MatchServiceError
    defaultService
    "TestFailoverNotAvailableFault"

-- |
_SubnetNotAllowedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "SubnetNotAllowedFault"

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
_ClusterQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceededFault"

-- |
_InvalidACLStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidACLStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidACLStateFault"

-- |
_ShardsPerClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ShardsPerClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ShardsPerClusterQuotaExceededFault"

-- |
_ServiceUpdateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUpdateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceUpdateNotFoundFault"

-- |
_SubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupNotFoundFault"

-- |
_SubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupAlreadyExistsFault"

-- |
_UserNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserNotFoundFault"

-- |
_NodeQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceededFault"

-- |
_SubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SubnetGroupQuotaExceededFault"

-- |
_DuplicateUserNameFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateUserNameFault =
  Core._MatchServiceError
    defaultService
    "DuplicateUserNameFault"

-- |
_ClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ClusterAlreadyExistsFault"

-- |
_UserQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserQuotaExceededFault"

-- |
_InvalidARNFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError
    defaultService
    "InvalidARNFault"

-- |
_NoOperationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoOperationFault =
  Core._MatchServiceError
    defaultService
    "NoOperationFault"

-- |
_InvalidCredentialsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCredentialsException =
  Core._MatchServiceError
    defaultService
    "InvalidCredentialsException"

-- |
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombinationException"
