{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MemoryDb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2021-01-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- MemoryDB for Redis is a fully managed, Redis-compatible, in-memory
-- database that delivers ultra-fast performance and Multi-AZ durability
-- for modern applications built using microservices architectures.
-- MemoryDB stores the entire database in-memory, enabling low latency and
-- high throughput data access. It is compatible with Redis, a popular open
-- source data store, enabling you to leverage Redis’ flexible and friendly
-- data structures, APIs, and commands.
module Amazonka.MemoryDb
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ACLAlreadyExistsFault
    _ACLAlreadyExistsFault,

    -- ** ACLNotFoundFault
    _ACLNotFoundFault,

    -- ** ACLQuotaExceededFault
    _ACLQuotaExceededFault,

    -- ** APICallRateForCustomerExceededFault
    _APICallRateForCustomerExceededFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** DefaultUserRequired
    _DefaultUserRequired,

    -- ** DuplicateUserNameFault
    _DuplicateUserNameFault,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** InvalidACLStateFault
    _InvalidACLStateFault,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** InvalidCredentialsException
    _InvalidCredentialsException,

    -- ** InvalidKMSKeyFault
    _InvalidKMSKeyFault,

    -- ** InvalidNodeStateFault
    _InvalidNodeStateFault,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** InvalidParameterGroupStateFault
    _InvalidParameterGroupStateFault,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidSnapshotStateFault
    _InvalidSnapshotStateFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** InvalidUserStateFault
    _InvalidUserStateFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** NoOperationFault
    _NoOperationFault,

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** ParameterGroupAlreadyExistsFault
    _ParameterGroupAlreadyExistsFault,

    -- ** ParameterGroupNotFoundFault
    _ParameterGroupNotFoundFault,

    -- ** ParameterGroupQuotaExceededFault
    _ParameterGroupQuotaExceededFault,

    -- ** ReservedNodeAlreadyExistsFault
    _ReservedNodeAlreadyExistsFault,

    -- ** ReservedNodeNotFoundFault
    _ReservedNodeNotFoundFault,

    -- ** ReservedNodeQuotaExceededFault
    _ReservedNodeQuotaExceededFault,

    -- ** ReservedNodesOfferingNotFoundFault
    _ReservedNodesOfferingNotFoundFault,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** ServiceUpdateNotFoundFault
    _ServiceUpdateNotFoundFault,

    -- ** ShardNotFoundFault
    _ShardNotFoundFault,

    -- ** ShardsPerClusterQuotaExceededFault
    _ShardsPerClusterQuotaExceededFault,

    -- ** SnapshotAlreadyExistsFault
    _SnapshotAlreadyExistsFault,

    -- ** SnapshotNotFoundFault
    _SnapshotNotFoundFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** SubnetGroupAlreadyExistsFault
    _SubnetGroupAlreadyExistsFault,

    -- ** SubnetGroupInUseFault
    _SubnetGroupInUseFault,

    -- ** SubnetGroupNotFoundFault
    _SubnetGroupNotFoundFault,

    -- ** SubnetGroupQuotaExceededFault
    _SubnetGroupQuotaExceededFault,

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** SubnetNotAllowedFault
    _SubnetNotAllowedFault,

    -- ** SubnetQuotaExceededFault
    _SubnetQuotaExceededFault,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** TestFailoverNotAvailableFault
    _TestFailoverNotAvailableFault,

    -- ** UserAlreadyExistsFault
    _UserAlreadyExistsFault,

    -- ** UserNotFoundFault
    _UserNotFoundFault,

    -- ** UserQuotaExceededFault
    _UserQuotaExceededFault,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchUpdateCluster
    BatchUpdateCluster (BatchUpdateCluster'),
    newBatchUpdateCluster,
    BatchUpdateClusterResponse (BatchUpdateClusterResponse'),
    newBatchUpdateClusterResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** CreateACL
    CreateACL (CreateACL'),
    newCreateACL,
    CreateACLResponse (CreateACLResponse'),
    newCreateACLResponse,

    -- ** CreateCluster
    CreateCluster (CreateCluster'),
    newCreateCluster,
    CreateClusterResponse (CreateClusterResponse'),
    newCreateClusterResponse,

    -- ** CreateParameterGroup
    CreateParameterGroup (CreateParameterGroup'),
    newCreateParameterGroup,
    CreateParameterGroupResponse (CreateParameterGroupResponse'),
    newCreateParameterGroupResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** CreateSubnetGroup
    CreateSubnetGroup (CreateSubnetGroup'),
    newCreateSubnetGroup,
    CreateSubnetGroupResponse (CreateSubnetGroupResponse'),
    newCreateSubnetGroupResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    CreateUserResponse (CreateUserResponse'),
    newCreateUserResponse,

    -- ** DeleteACL
    DeleteACL (DeleteACL'),
    newDeleteACL,
    DeleteACLResponse (DeleteACLResponse'),
    newDeleteACLResponse,

    -- ** DeleteCluster
    DeleteCluster (DeleteCluster'),
    newDeleteCluster,
    DeleteClusterResponse (DeleteClusterResponse'),
    newDeleteClusterResponse,

    -- ** DeleteParameterGroup
    DeleteParameterGroup (DeleteParameterGroup'),
    newDeleteParameterGroup,
    DeleteParameterGroupResponse (DeleteParameterGroupResponse'),
    newDeleteParameterGroupResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeleteSubnetGroup
    DeleteSubnetGroup (DeleteSubnetGroup'),
    newDeleteSubnetGroup,
    DeleteSubnetGroupResponse (DeleteSubnetGroupResponse'),
    newDeleteSubnetGroupResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    DeleteUserResponse (DeleteUserResponse'),
    newDeleteUserResponse,

    -- ** DescribeACLs (Paginated)
    DescribeACLs (DescribeACLs'),
    newDescribeACLs,
    DescribeACLsResponse (DescribeACLsResponse'),
    newDescribeACLsResponse,

    -- ** DescribeClusters (Paginated)
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** DescribeEngineVersions (Paginated)
    DescribeEngineVersions (DescribeEngineVersions'),
    newDescribeEngineVersions,
    DescribeEngineVersionsResponse (DescribeEngineVersionsResponse'),
    newDescribeEngineVersionsResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeParameterGroups (Paginated)
    DescribeParameterGroups (DescribeParameterGroups'),
    newDescribeParameterGroups,
    DescribeParameterGroupsResponse (DescribeParameterGroupsResponse'),
    newDescribeParameterGroupsResponse,

    -- ** DescribeParameters (Paginated)
    DescribeParameters (DescribeParameters'),
    newDescribeParameters,
    DescribeParametersResponse (DescribeParametersResponse'),
    newDescribeParametersResponse,

    -- ** DescribeReservedNodes (Paginated)
    DescribeReservedNodes (DescribeReservedNodes'),
    newDescribeReservedNodes,
    DescribeReservedNodesResponse (DescribeReservedNodesResponse'),
    newDescribeReservedNodesResponse,

    -- ** DescribeReservedNodesOfferings (Paginated)
    DescribeReservedNodesOfferings (DescribeReservedNodesOfferings'),
    newDescribeReservedNodesOfferings,
    DescribeReservedNodesOfferingsResponse (DescribeReservedNodesOfferingsResponse'),
    newDescribeReservedNodesOfferingsResponse,

    -- ** DescribeServiceUpdates (Paginated)
    DescribeServiceUpdates (DescribeServiceUpdates'),
    newDescribeServiceUpdates,
    DescribeServiceUpdatesResponse (DescribeServiceUpdatesResponse'),
    newDescribeServiceUpdatesResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DescribeSubnetGroups (Paginated)
    DescribeSubnetGroups (DescribeSubnetGroups'),
    newDescribeSubnetGroups,
    DescribeSubnetGroupsResponse (DescribeSubnetGroupsResponse'),
    newDescribeSubnetGroupsResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** FailoverShard
    FailoverShard (FailoverShard'),
    newFailoverShard,
    FailoverShardResponse (FailoverShardResponse'),
    newFailoverShardResponse,

    -- ** ListAllowedNodeTypeUpdates
    ListAllowedNodeTypeUpdates (ListAllowedNodeTypeUpdates'),
    newListAllowedNodeTypeUpdates,
    ListAllowedNodeTypeUpdatesResponse (ListAllowedNodeTypeUpdatesResponse'),
    newListAllowedNodeTypeUpdatesResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** PurchaseReservedNodesOffering
    PurchaseReservedNodesOffering (PurchaseReservedNodesOffering'),
    newPurchaseReservedNodesOffering,
    PurchaseReservedNodesOfferingResponse (PurchaseReservedNodesOfferingResponse'),
    newPurchaseReservedNodesOfferingResponse,

    -- ** ResetParameterGroup
    ResetParameterGroup (ResetParameterGroup'),
    newResetParameterGroup,
    ResetParameterGroupResponse (ResetParameterGroupResponse'),
    newResetParameterGroupResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateACL
    UpdateACL (UpdateACL'),
    newUpdateACL,
    UpdateACLResponse (UpdateACLResponse'),
    newUpdateACLResponse,

    -- ** UpdateCluster
    UpdateCluster (UpdateCluster'),
    newUpdateCluster,
    UpdateClusterResponse (UpdateClusterResponse'),
    newUpdateClusterResponse,

    -- ** UpdateParameterGroup
    UpdateParameterGroup (UpdateParameterGroup'),
    newUpdateParameterGroup,
    UpdateParameterGroupResponse (UpdateParameterGroupResponse'),
    newUpdateParameterGroupResponse,

    -- ** UpdateSubnetGroup
    UpdateSubnetGroup (UpdateSubnetGroup'),
    newUpdateSubnetGroup,
    UpdateSubnetGroupResponse (UpdateSubnetGroupResponse'),
    newUpdateSubnetGroupResponse,

    -- ** UpdateUser
    UpdateUser (UpdateUser'),
    newUpdateUser,
    UpdateUserResponse (UpdateUserResponse'),
    newUpdateUserResponse,

    -- * Types

    -- ** AZStatus
    AZStatus (..),

    -- ** AuthenticationType
    AuthenticationType (..),

    -- ** DataTieringStatus
    DataTieringStatus (..),

    -- ** InputAuthenticationType
    InputAuthenticationType (..),

    -- ** ServiceUpdateStatus
    ServiceUpdateStatus (..),

    -- ** ServiceUpdateType
    ServiceUpdateType (..),

    -- ** SourceType
    SourceType (..),

    -- ** ACL
    ACL (ACL'),
    newACL,

    -- ** ACLPendingChanges
    ACLPendingChanges (ACLPendingChanges'),
    newACLPendingChanges,

    -- ** ACLsUpdateStatus
    ACLsUpdateStatus (ACLsUpdateStatus'),
    newACLsUpdateStatus,

    -- ** Authentication
    Authentication (Authentication'),
    newAuthentication,

    -- ** AuthenticationMode
    AuthenticationMode (AuthenticationMode'),
    newAuthenticationMode,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** Cluster
    Cluster (Cluster'),
    newCluster,

    -- ** ClusterConfiguration
    ClusterConfiguration (ClusterConfiguration'),
    newClusterConfiguration,

    -- ** ClusterPendingUpdates
    ClusterPendingUpdates (ClusterPendingUpdates'),
    newClusterPendingUpdates,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EngineVersionInfo
    EngineVersionInfo (EngineVersionInfo'),
    newEngineVersionInfo,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** Node
    Node (Node'),
    newNode,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** ParameterGroup
    ParameterGroup (ParameterGroup'),
    newParameterGroup,

    -- ** ParameterNameValue
    ParameterNameValue (ParameterNameValue'),
    newParameterNameValue,

    -- ** PendingModifiedServiceUpdate
    PendingModifiedServiceUpdate (PendingModifiedServiceUpdate'),
    newPendingModifiedServiceUpdate,

    -- ** RecurringCharge
    RecurringCharge (RecurringCharge'),
    newRecurringCharge,

    -- ** ReplicaConfigurationRequest
    ReplicaConfigurationRequest (ReplicaConfigurationRequest'),
    newReplicaConfigurationRequest,

    -- ** ReservedNode
    ReservedNode (ReservedNode'),
    newReservedNode,

    -- ** ReservedNodesOffering
    ReservedNodesOffering (ReservedNodesOffering'),
    newReservedNodesOffering,

    -- ** ReshardingStatus
    ReshardingStatus (ReshardingStatus'),
    newReshardingStatus,

    -- ** SecurityGroupMembership
    SecurityGroupMembership (SecurityGroupMembership'),
    newSecurityGroupMembership,

    -- ** ServiceUpdate
    ServiceUpdate (ServiceUpdate'),
    newServiceUpdate,

    -- ** ServiceUpdateRequest
    ServiceUpdateRequest (ServiceUpdateRequest'),
    newServiceUpdateRequest,

    -- ** Shard
    Shard (Shard'),
    newShard,

    -- ** ShardConfiguration
    ShardConfiguration (ShardConfiguration'),
    newShardConfiguration,

    -- ** ShardConfigurationRequest
    ShardConfigurationRequest (ShardConfigurationRequest'),
    newShardConfigurationRequest,

    -- ** ShardDetail
    ShardDetail (ShardDetail'),
    newShardDetail,

    -- ** SlotMigration
    SlotMigration (SlotMigration'),
    newSlotMigration,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SubnetGroup
    SubnetGroup (SubnetGroup'),
    newSubnetGroup,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** UnprocessedCluster
    UnprocessedCluster (UnprocessedCluster'),
    newUnprocessedCluster,

    -- ** User
    User (User'),
    newUser,
  )
where

import Amazonka.MemoryDb.BatchUpdateCluster
import Amazonka.MemoryDb.CopySnapshot
import Amazonka.MemoryDb.CreateACL
import Amazonka.MemoryDb.CreateCluster
import Amazonka.MemoryDb.CreateParameterGroup
import Amazonka.MemoryDb.CreateSnapshot
import Amazonka.MemoryDb.CreateSubnetGroup
import Amazonka.MemoryDb.CreateUser
import Amazonka.MemoryDb.DeleteACL
import Amazonka.MemoryDb.DeleteCluster
import Amazonka.MemoryDb.DeleteParameterGroup
import Amazonka.MemoryDb.DeleteSnapshot
import Amazonka.MemoryDb.DeleteSubnetGroup
import Amazonka.MemoryDb.DeleteUser
import Amazonka.MemoryDb.DescribeACLs
import Amazonka.MemoryDb.DescribeClusters
import Amazonka.MemoryDb.DescribeEngineVersions
import Amazonka.MemoryDb.DescribeEvents
import Amazonka.MemoryDb.DescribeParameterGroups
import Amazonka.MemoryDb.DescribeParameters
import Amazonka.MemoryDb.DescribeReservedNodes
import Amazonka.MemoryDb.DescribeReservedNodesOfferings
import Amazonka.MemoryDb.DescribeServiceUpdates
import Amazonka.MemoryDb.DescribeSnapshots
import Amazonka.MemoryDb.DescribeSubnetGroups
import Amazonka.MemoryDb.DescribeUsers
import Amazonka.MemoryDb.FailoverShard
import Amazonka.MemoryDb.Lens
import Amazonka.MemoryDb.ListAllowedNodeTypeUpdates
import Amazonka.MemoryDb.ListTags
import Amazonka.MemoryDb.PurchaseReservedNodesOffering
import Amazonka.MemoryDb.ResetParameterGroup
import Amazonka.MemoryDb.TagResource
import Amazonka.MemoryDb.Types
import Amazonka.MemoryDb.UntagResource
import Amazonka.MemoryDb.UpdateACL
import Amazonka.MemoryDb.UpdateCluster
import Amazonka.MemoryDb.UpdateParameterGroup
import Amazonka.MemoryDb.UpdateSubnetGroup
import Amazonka.MemoryDb.UpdateUser
import Amazonka.MemoryDb.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MemoryDb'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
