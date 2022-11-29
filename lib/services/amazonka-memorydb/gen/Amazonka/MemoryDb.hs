{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MemoryDb
-- Copyright   : (c) 2013-2022 Brendan Hay
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

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** SubnetQuotaExceededFault
    _SubnetQuotaExceededFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** ServiceUpdateNotFoundFault
    _ServiceUpdateNotFoundFault,

    -- ** UserNotFoundFault
    _UserNotFoundFault,

    -- ** ACLNotFoundFault
    _ACLNotFoundFault,

    -- ** ShardNotFoundFault
    _ShardNotFoundFault,

    -- ** ClusterNotFoundFault
    _ClusterNotFoundFault,

    -- ** InvalidSnapshotStateFault
    _InvalidSnapshotStateFault,

    -- ** NoOperationFault
    _NoOperationFault,

    -- ** UserAlreadyExistsFault
    _UserAlreadyExistsFault,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** SubnetGroupInUseFault
    _SubnetGroupInUseFault,

    -- ** ClusterAlreadyExistsFault
    _ClusterAlreadyExistsFault,

    -- ** ACLAlreadyExistsFault
    _ACLAlreadyExistsFault,

    -- ** DefaultUserRequired
    _DefaultUserRequired,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** ParameterGroupAlreadyExistsFault
    _ParameterGroupAlreadyExistsFault,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** ShardsPerClusterQuotaExceededFault
    _ShardsPerClusterQuotaExceededFault,

    -- ** InvalidParameterGroupStateFault
    _InvalidParameterGroupStateFault,

    -- ** UserQuotaExceededFault
    _UserQuotaExceededFault,

    -- ** DuplicateUserNameFault
    _DuplicateUserNameFault,

    -- ** ParameterGroupNotFoundFault
    _ParameterGroupNotFoundFault,

    -- ** InvalidACLStateFault
    _InvalidACLStateFault,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** SnapshotAlreadyExistsFault
    _SnapshotAlreadyExistsFault,

    -- ** InvalidNodeStateFault
    _InvalidNodeStateFault,

    -- ** TestFailoverNotAvailableFault
    _TestFailoverNotAvailableFault,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** SubnetGroupNotFoundFault
    _SubnetGroupNotFoundFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** InvalidUserStateFault
    _InvalidUserStateFault,

    -- ** SnapshotNotFoundFault
    _SnapshotNotFoundFault,

    -- ** InvalidClusterStateFault
    _InvalidClusterStateFault,

    -- ** InvalidKMSKeyFault
    _InvalidKMSKeyFault,

    -- ** SubnetGroupAlreadyExistsFault
    _SubnetGroupAlreadyExistsFault,

    -- ** ParameterGroupQuotaExceededFault
    _ParameterGroupQuotaExceededFault,

    -- ** InsufficientClusterCapacityFault
    _InsufficientClusterCapacityFault,

    -- ** ACLQuotaExceededFault
    _ACLQuotaExceededFault,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidCredentialsException
    _InvalidCredentialsException,

    -- ** SubnetNotAllowedFault
    _SubnetNotAllowedFault,

    -- ** APICallRateForCustomerExceededFault
    _APICallRateForCustomerExceededFault,

    -- ** SubnetGroupQuotaExceededFault
    _SubnetGroupQuotaExceededFault,

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

    -- ** DescribeACLs
    DescribeACLs (DescribeACLs'),
    newDescribeACLs,
    DescribeACLsResponse (DescribeACLsResponse'),
    newDescribeACLsResponse,

    -- ** DescribeClusters
    DescribeClusters (DescribeClusters'),
    newDescribeClusters,
    DescribeClustersResponse (DescribeClustersResponse'),
    newDescribeClustersResponse,

    -- ** DescribeEngineVersions
    DescribeEngineVersions (DescribeEngineVersions'),
    newDescribeEngineVersions,
    DescribeEngineVersionsResponse (DescribeEngineVersionsResponse'),
    newDescribeEngineVersionsResponse,

    -- ** DescribeEvents
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeParameterGroups
    DescribeParameterGroups (DescribeParameterGroups'),
    newDescribeParameterGroups,
    DescribeParameterGroupsResponse (DescribeParameterGroupsResponse'),
    newDescribeParameterGroupsResponse,

    -- ** DescribeParameters
    DescribeParameters (DescribeParameters'),
    newDescribeParameters,
    DescribeParametersResponse (DescribeParametersResponse'),
    newDescribeParametersResponse,

    -- ** DescribeServiceUpdates
    DescribeServiceUpdates (DescribeServiceUpdates'),
    newDescribeServiceUpdates,
    DescribeServiceUpdatesResponse (DescribeServiceUpdatesResponse'),
    newDescribeServiceUpdatesResponse,

    -- ** DescribeSnapshots
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DescribeSubnetGroups
    DescribeSubnetGroups (DescribeSubnetGroups'),
    newDescribeSubnetGroups,
    DescribeSubnetGroupsResponse (DescribeSubnetGroupsResponse'),
    newDescribeSubnetGroupsResponse,

    -- ** DescribeUsers
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

    -- ** ReplicaConfigurationRequest
    ReplicaConfigurationRequest (ReplicaConfigurationRequest'),
    newReplicaConfigurationRequest,

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
import Amazonka.MemoryDb.DescribeServiceUpdates
import Amazonka.MemoryDb.DescribeSnapshots
import Amazonka.MemoryDb.DescribeSubnetGroups
import Amazonka.MemoryDb.DescribeUsers
import Amazonka.MemoryDb.FailoverShard
import Amazonka.MemoryDb.Lens
import Amazonka.MemoryDb.ListAllowedNodeTypeUpdates
import Amazonka.MemoryDb.ListTags
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
