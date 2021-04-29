{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon ElastiCache
--
-- Amazon ElastiCache is a web service that makes it easier to set up,
-- operate, and scale a distributed cache in the cloud.
--
-- With ElastiCache, customers get all of the benefits of a
-- high-performance, in-memory cache with less of the administrative burden
-- involved in launching and managing a distributed cache. The service
-- makes setup, scaling, and cluster failure handling much simpler than in
-- a self-managed cache deployment.
--
-- In addition, through integration with Amazon CloudWatch, customers get
-- enhanced visibility into the key performance statistics associated with
-- their cache and can receive alarms if a part of their cache runs hot.
module Network.AWS.ElastiCache
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** CacheSubnetGroupQuotaExceededFault
    _CacheSubnetGroupQuotaExceededFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** CacheClusterNotFoundFault
    _CacheClusterNotFoundFault,

    -- ** InvalidReplicationGroupStateFault
    _InvalidReplicationGroupStateFault,

    -- ** SubnetNotAllowedFault
    _SubnetNotAllowedFault,

    -- ** CacheSubnetGroupAlreadyExistsFault
    _CacheSubnetGroupAlreadyExistsFault,

    -- ** TestFailoverNotAvailableFault
    _TestFailoverNotAvailableFault,

    -- ** GlobalReplicationGroupAlreadyExistsFault
    _GlobalReplicationGroupAlreadyExistsFault,

    -- ** SnapshotFeatureNotSupportedFault
    _SnapshotFeatureNotSupportedFault,

    -- ** UserGroupQuotaExceededFault
    _UserGroupQuotaExceededFault,

    -- ** InvalidKMSKeyFault
    _InvalidKMSKeyFault,

    -- ** InvalidCacheSecurityGroupStateFault
    _InvalidCacheSecurityGroupStateFault,

    -- ** ReservedCacheNodeNotFoundFault
    _ReservedCacheNodeNotFoundFault,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** APICallRateForCustomerExceededFault
    _APICallRateForCustomerExceededFault,

    -- ** InvalidCacheParameterGroupStateFault
    _InvalidCacheParameterGroupStateFault,

    -- ** UserQuotaExceededFault
    _UserQuotaExceededFault,

    -- ** NoOperationFault
    _NoOperationFault,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** CacheParameterGroupAlreadyExistsFault
    _CacheParameterGroupAlreadyExistsFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** CacheSubnetQuotaExceededFault
    _CacheSubnetQuotaExceededFault,

    -- ** SnapshotAlreadyExistsFault
    _SnapshotAlreadyExistsFault,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** UserNotFoundFault
    _UserNotFoundFault,

    -- ** InsufficientCacheClusterCapacityFault
    _InsufficientCacheClusterCapacityFault,

    -- ** InvalidUserStateFault
    _InvalidUserStateFault,

    -- ** ServiceUpdateNotFoundFault
    _ServiceUpdateNotFoundFault,

    -- ** SnapshotNotFoundFault
    _SnapshotNotFoundFault,

    -- ** UserGroupAlreadyExistsFault
    _UserGroupAlreadyExistsFault,

    -- ** InvalidUserGroupStateFault
    _InvalidUserGroupStateFault,

    -- ** InvalidCacheClusterStateFault
    _InvalidCacheClusterStateFault,

    -- ** UserGroupNotFoundFault
    _UserGroupNotFoundFault,

    -- ** NodeGroupsPerReplicationGroupQuotaExceededFault
    _NodeGroupsPerReplicationGroupQuotaExceededFault,

    -- ** ReplicationGroupAlreadyExistsFault
    _ReplicationGroupAlreadyExistsFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** ReservedCacheNodeQuotaExceededFault
    _ReservedCacheNodeQuotaExceededFault,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** CacheSecurityGroupNotFoundFault
    _CacheSecurityGroupNotFoundFault,

    -- ** ReservedCacheNodeAlreadyExistsFault
    _ReservedCacheNodeAlreadyExistsFault,

    -- ** CacheSubnetGroupInUse
    _CacheSubnetGroupInUse,

    -- ** InvalidGlobalReplicationGroupStateFault
    _InvalidGlobalReplicationGroupStateFault,

    -- ** GlobalReplicationGroupNotFoundFault
    _GlobalReplicationGroupNotFoundFault,

    -- ** CacheSubnetGroupNotFoundFault
    _CacheSubnetGroupNotFoundFault,

    -- ** CacheSecurityGroupAlreadyExistsFault
    _CacheSecurityGroupAlreadyExistsFault,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** NodeGroupNotFoundFault
    _NodeGroupNotFoundFault,

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** ReplicationGroupAlreadyUnderMigrationFault
    _ReplicationGroupAlreadyUnderMigrationFault,

    -- ** DefaultUserRequired
    _DefaultUserRequired,

    -- ** CacheParameterGroupNotFoundFault
    _CacheParameterGroupNotFoundFault,

    -- ** DuplicateUserNameFault
    _DuplicateUserNameFault,

    -- ** InvalidSnapshotStateFault
    _InvalidSnapshotStateFault,

    -- ** ReplicationGroupNotUnderMigrationFault
    _ReplicationGroupNotUnderMigrationFault,

    -- ** UserAlreadyExistsFault
    _UserAlreadyExistsFault,

    -- ** DefaultUserAssociatedToUserGroupFault
    _DefaultUserAssociatedToUserGroupFault,

    -- ** CacheParameterGroupQuotaExceededFault
    _CacheParameterGroupQuotaExceededFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** ReservedCacheNodesOfferingNotFoundFault
    _ReservedCacheNodesOfferingNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** ReplicationGroupNotFoundFault
    _ReplicationGroupNotFoundFault,

    -- ** CacheClusterAlreadyExistsFault
    _CacheClusterAlreadyExistsFault,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** CacheSecurityGroupQuotaExceededFault
    _CacheSecurityGroupQuotaExceededFault,

    -- * Waiters
    -- $waiters

    -- ** ReplicationGroupDeleted
    newReplicationGroupDeleted,

    -- ** ReplicationGroupAvailable
    newReplicationGroupAvailable,

    -- ** CacheClusterAvailable
    newCacheClusterAvailable,

    -- ** CacheClusterDeleted
    newCacheClusterDeleted,

    -- * Operations
    -- $operations

    -- ** CreateReplicationGroup
    CreateReplicationGroup (CreateReplicationGroup'),
    newCreateReplicationGroup,
    CreateReplicationGroupResponse (CreateReplicationGroupResponse'),
    newCreateReplicationGroupResponse,

    -- ** DeleteCacheCluster
    DeleteCacheCluster (DeleteCacheCluster'),
    newDeleteCacheCluster,
    DeleteCacheClusterResponse (DeleteCacheClusterResponse'),
    newDeleteCacheClusterResponse,

    -- ** RebootCacheCluster
    RebootCacheCluster (RebootCacheCluster'),
    newRebootCacheCluster,
    RebootCacheClusterResponse (RebootCacheClusterResponse'),
    newRebootCacheClusterResponse,

    -- ** IncreaseNodeGroupsInGlobalReplicationGroup
    IncreaseNodeGroupsInGlobalReplicationGroup (IncreaseNodeGroupsInGlobalReplicationGroup'),
    newIncreaseNodeGroupsInGlobalReplicationGroup,
    IncreaseNodeGroupsInGlobalReplicationGroupResponse (IncreaseNodeGroupsInGlobalReplicationGroupResponse'),
    newIncreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** DeleteUserGroup
    DeleteUserGroup (DeleteUserGroup'),
    newDeleteUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

    -- ** DeleteCacheSecurityGroup
    DeleteCacheSecurityGroup (DeleteCacheSecurityGroup'),
    newDeleteCacheSecurityGroup,
    DeleteCacheSecurityGroupResponse (DeleteCacheSecurityGroupResponse'),
    newDeleteCacheSecurityGroupResponse,

    -- ** StartMigration
    StartMigration (StartMigration'),
    newStartMigration,
    StartMigrationResponse (StartMigrationResponse'),
    newStartMigrationResponse,

    -- ** AuthorizeCacheSecurityGroupIngress
    AuthorizeCacheSecurityGroupIngress (AuthorizeCacheSecurityGroupIngress'),
    newAuthorizeCacheSecurityGroupIngress,
    AuthorizeCacheSecurityGroupIngressResponse (AuthorizeCacheSecurityGroupIngressResponse'),
    newAuthorizeCacheSecurityGroupIngressResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** DecreaseReplicaCount
    DecreaseReplicaCount (DecreaseReplicaCount'),
    newDecreaseReplicaCount,
    DecreaseReplicaCountResponse (DecreaseReplicaCountResponse'),
    newDecreaseReplicaCountResponse,

    -- ** RebalanceSlotsInGlobalReplicationGroup
    RebalanceSlotsInGlobalReplicationGroup (RebalanceSlotsInGlobalReplicationGroup'),
    newRebalanceSlotsInGlobalReplicationGroup,
    RebalanceSlotsInGlobalReplicationGroupResponse (RebalanceSlotsInGlobalReplicationGroupResponse'),
    newRebalanceSlotsInGlobalReplicationGroupResponse,

    -- ** CreateCacheSecurityGroup
    CreateCacheSecurityGroup (CreateCacheSecurityGroup'),
    newCreateCacheSecurityGroup,
    CreateCacheSecurityGroupResponse (CreateCacheSecurityGroupResponse'),
    newCreateCacheSecurityGroupResponse,

    -- ** DescribeCacheSubnetGroups (Paginated)
    DescribeCacheSubnetGroups (DescribeCacheSubnetGroups'),
    newDescribeCacheSubnetGroups,
    DescribeCacheSubnetGroupsResponse (DescribeCacheSubnetGroupsResponse'),
    newDescribeCacheSubnetGroupsResponse,

    -- ** DescribeGlobalReplicationGroups (Paginated)
    DescribeGlobalReplicationGroups (DescribeGlobalReplicationGroups'),
    newDescribeGlobalReplicationGroups,
    DescribeGlobalReplicationGroupsResponse (DescribeGlobalReplicationGroupsResponse'),
    newDescribeGlobalReplicationGroupsResponse,

    -- ** ModifyCacheCluster
    ModifyCacheCluster (ModifyCacheCluster'),
    newModifyCacheCluster,
    ModifyCacheClusterResponse (ModifyCacheClusterResponse'),
    newModifyCacheClusterResponse,

    -- ** DescribeReservedCacheNodes (Paginated)
    DescribeReservedCacheNodes (DescribeReservedCacheNodes'),
    newDescribeReservedCacheNodes,
    DescribeReservedCacheNodesResponse (DescribeReservedCacheNodesResponse'),
    newDescribeReservedCacheNodesResponse,

    -- ** DeleteCacheParameterGroup
    DeleteCacheParameterGroup (DeleteCacheParameterGroup'),
    newDeleteCacheParameterGroup,
    DeleteCacheParameterGroupResponse (DeleteCacheParameterGroupResponse'),
    newDeleteCacheParameterGroupResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** DescribeCacheSecurityGroups (Paginated)
    DescribeCacheSecurityGroups (DescribeCacheSecurityGroups'),
    newDescribeCacheSecurityGroups,
    DescribeCacheSecurityGroupsResponse (DescribeCacheSecurityGroupsResponse'),
    newDescribeCacheSecurityGroupsResponse,

    -- ** BatchStopUpdateAction
    BatchStopUpdateAction (BatchStopUpdateAction'),
    newBatchStopUpdateAction,
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

    -- ** ModifyReplicationGroup
    ModifyReplicationGroup (ModifyReplicationGroup'),
    newModifyReplicationGroup,
    ModifyReplicationGroupResponse (ModifyReplicationGroupResponse'),
    newModifyReplicationGroupResponse,

    -- ** PurchaseReservedCacheNodesOffering
    PurchaseReservedCacheNodesOffering (PurchaseReservedCacheNodesOffering'),
    newPurchaseReservedCacheNodesOffering,
    PurchaseReservedCacheNodesOfferingResponse (PurchaseReservedCacheNodesOfferingResponse'),
    newPurchaseReservedCacheNodesOfferingResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    User (User'),
    newUser,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** CompleteMigration
    CompleteMigration (CompleteMigration'),
    newCompleteMigration,
    CompleteMigrationResponse (CompleteMigrationResponse'),
    newCompleteMigrationResponse,

    -- ** CreateCacheCluster
    CreateCacheCluster (CreateCacheCluster'),
    newCreateCacheCluster,
    CreateCacheClusterResponse (CreateCacheClusterResponse'),
    newCreateCacheClusterResponse,

    -- ** DisassociateGlobalReplicationGroup
    DisassociateGlobalReplicationGroup (DisassociateGlobalReplicationGroup'),
    newDisassociateGlobalReplicationGroup,
    DisassociateGlobalReplicationGroupResponse (DisassociateGlobalReplicationGroupResponse'),
    newDisassociateGlobalReplicationGroupResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DeleteReplicationGroup
    DeleteReplicationGroup (DeleteReplicationGroup'),
    newDeleteReplicationGroup,
    DeleteReplicationGroupResponse (DeleteReplicationGroupResponse'),
    newDeleteReplicationGroupResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** TestFailover
    TestFailover (TestFailover'),
    newTestFailover,
    TestFailoverResponse (TestFailoverResponse'),
    newTestFailoverResponse,

    -- ** BatchApplyUpdateAction
    BatchApplyUpdateAction (BatchApplyUpdateAction'),
    newBatchApplyUpdateAction,
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

    -- ** IncreaseReplicaCount
    IncreaseReplicaCount (IncreaseReplicaCount'),
    newIncreaseReplicaCount,
    IncreaseReplicaCountResponse (IncreaseReplicaCountResponse'),
    newIncreaseReplicaCountResponse,

    -- ** ModifyReplicationGroupShardConfiguration
    ModifyReplicationGroupShardConfiguration (ModifyReplicationGroupShardConfiguration'),
    newModifyReplicationGroupShardConfiguration,
    ModifyReplicationGroupShardConfigurationResponse (ModifyReplicationGroupShardConfigurationResponse'),
    newModifyReplicationGroupShardConfigurationResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** ListAllowedNodeTypeModifications
    ListAllowedNodeTypeModifications (ListAllowedNodeTypeModifications'),
    newListAllowedNodeTypeModifications,
    ListAllowedNodeTypeModificationsResponse (ListAllowedNodeTypeModificationsResponse'),
    newListAllowedNodeTypeModificationsResponse,

    -- ** ResetCacheParameterGroup
    ResetCacheParameterGroup (ResetCacheParameterGroup'),
    newResetCacheParameterGroup,
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

    -- ** CreateCacheSubnetGroup
    CreateCacheSubnetGroup (CreateCacheSubnetGroup'),
    newCreateCacheSubnetGroup,
    CreateCacheSubnetGroupResponse (CreateCacheSubnetGroupResponse'),
    newCreateCacheSubnetGroupResponse,

    -- ** CreateGlobalReplicationGroup
    CreateGlobalReplicationGroup (CreateGlobalReplicationGroup'),
    newCreateGlobalReplicationGroup,
    CreateGlobalReplicationGroupResponse (CreateGlobalReplicationGroupResponse'),
    newCreateGlobalReplicationGroupResponse,

    -- ** DescribeCacheParameterGroups (Paginated)
    DescribeCacheParameterGroups (DescribeCacheParameterGroups'),
    newDescribeCacheParameterGroups,
    DescribeCacheParameterGroupsResponse (DescribeCacheParameterGroupsResponse'),
    newDescribeCacheParameterGroupsResponse,

    -- ** FailoverGlobalReplicationGroup
    FailoverGlobalReplicationGroup (FailoverGlobalReplicationGroup'),
    newFailoverGlobalReplicationGroup,
    FailoverGlobalReplicationGroupResponse (FailoverGlobalReplicationGroupResponse'),
    newFailoverGlobalReplicationGroupResponse,

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** DecreaseNodeGroupsInGlobalReplicationGroup
    DecreaseNodeGroupsInGlobalReplicationGroup (DecreaseNodeGroupsInGlobalReplicationGroup'),
    newDecreaseNodeGroupsInGlobalReplicationGroup,
    DecreaseNodeGroupsInGlobalReplicationGroupResponse (DecreaseNodeGroupsInGlobalReplicationGroupResponse'),
    newDecreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** DescribeUpdateActions (Paginated)
    DescribeUpdateActions (DescribeUpdateActions'),
    newDescribeUpdateActions,
    DescribeUpdateActionsResponse (DescribeUpdateActionsResponse'),
    newDescribeUpdateActionsResponse,

    -- ** ModifyUser
    ModifyUser (ModifyUser'),
    newModifyUser,
    User (User'),
    newUser,

    -- ** DeleteCacheSubnetGroup
    DeleteCacheSubnetGroup (DeleteCacheSubnetGroup'),
    newDeleteCacheSubnetGroup,
    DeleteCacheSubnetGroupResponse (DeleteCacheSubnetGroupResponse'),
    newDeleteCacheSubnetGroupResponse,

    -- ** DeleteGlobalReplicationGroup
    DeleteGlobalReplicationGroup (DeleteGlobalReplicationGroup'),
    newDeleteGlobalReplicationGroup,
    DeleteGlobalReplicationGroupResponse (DeleteGlobalReplicationGroupResponse'),
    newDeleteGlobalReplicationGroupResponse,

    -- ** CreateCacheParameterGroup
    CreateCacheParameterGroup (CreateCacheParameterGroup'),
    newCreateCacheParameterGroup,
    CreateCacheParameterGroupResponse (CreateCacheParameterGroupResponse'),
    newCreateCacheParameterGroupResponse,

    -- ** DescribeCacheEngineVersions (Paginated)
    DescribeCacheEngineVersions (DescribeCacheEngineVersions'),
    newDescribeCacheEngineVersions,
    DescribeCacheEngineVersionsResponse (DescribeCacheEngineVersionsResponse'),
    newDescribeCacheEngineVersionsResponse,

    -- ** ModifyUserGroup
    ModifyUserGroup (ModifyUserGroup'),
    newModifyUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

    -- ** DescribeCacheParameters (Paginated)
    DescribeCacheParameters (DescribeCacheParameters'),
    newDescribeCacheParameters,
    DescribeCacheParametersResponse (DescribeCacheParametersResponse'),
    newDescribeCacheParametersResponse,

    -- ** ModifyGlobalReplicationGroup
    ModifyGlobalReplicationGroup (ModifyGlobalReplicationGroup'),
    newModifyGlobalReplicationGroup,
    ModifyGlobalReplicationGroupResponse (ModifyGlobalReplicationGroupResponse'),
    newModifyGlobalReplicationGroupResponse,

    -- ** ModifyCacheSubnetGroup
    ModifyCacheSubnetGroup (ModifyCacheSubnetGroup'),
    newModifyCacheSubnetGroup,
    ModifyCacheSubnetGroupResponse (ModifyCacheSubnetGroupResponse'),
    newModifyCacheSubnetGroupResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    User (User'),
    newUser,

    -- ** DescribeUserGroups (Paginated)
    DescribeUserGroups (DescribeUserGroups'),
    newDescribeUserGroups,
    DescribeUserGroupsResponse (DescribeUserGroupsResponse'),
    newDescribeUserGroupsResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** DescribeCacheClusters (Paginated)
    DescribeCacheClusters (DescribeCacheClusters'),
    newDescribeCacheClusters,
    DescribeCacheClustersResponse (DescribeCacheClustersResponse'),
    newDescribeCacheClustersResponse,

    -- ** DescribeReservedCacheNodesOfferings (Paginated)
    DescribeReservedCacheNodesOfferings (DescribeReservedCacheNodesOfferings'),
    newDescribeReservedCacheNodesOfferings,
    DescribeReservedCacheNodesOfferingsResponse (DescribeReservedCacheNodesOfferingsResponse'),
    newDescribeReservedCacheNodesOfferingsResponse,

    -- ** DescribeReplicationGroups (Paginated)
    DescribeReplicationGroups (DescribeReplicationGroups'),
    newDescribeReplicationGroups,
    DescribeReplicationGroupsResponse (DescribeReplicationGroupsResponse'),
    newDescribeReplicationGroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** ModifyCacheParameterGroup
    ModifyCacheParameterGroup (ModifyCacheParameterGroup'),
    newModifyCacheParameterGroup,
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

    -- ** DescribeServiceUpdates (Paginated)
    DescribeServiceUpdates (DescribeServiceUpdates'),
    newDescribeServiceUpdates,
    DescribeServiceUpdatesResponse (DescribeServiceUpdatesResponse'),
    newDescribeServiceUpdatesResponse,

    -- ** DescribeEngineDefaultParameters (Paginated)
    DescribeEngineDefaultParameters (DescribeEngineDefaultParameters'),
    newDescribeEngineDefaultParameters,
    DescribeEngineDefaultParametersResponse (DescribeEngineDefaultParametersResponse'),
    newDescribeEngineDefaultParametersResponse,

    -- ** CreateUserGroup
    CreateUserGroup (CreateUserGroup'),
    newCreateUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

    -- ** RevokeCacheSecurityGroupIngress
    RevokeCacheSecurityGroupIngress (RevokeCacheSecurityGroupIngress'),
    newRevokeCacheSecurityGroupIngress,
    RevokeCacheSecurityGroupIngressResponse (RevokeCacheSecurityGroupIngressResponse'),
    newRevokeCacheSecurityGroupIngressResponse,

    -- * Types

    -- ** AZMode
    AZMode (..),

    -- ** AuthTokenUpdateStatus
    AuthTokenUpdateStatus (..),

    -- ** AuthTokenUpdateStrategyType
    AuthTokenUpdateStrategyType (..),

    -- ** AuthenticationType
    AuthenticationType (..),

    -- ** AutomaticFailoverStatus
    AutomaticFailoverStatus (..),

    -- ** ChangeType
    ChangeType (..),

    -- ** MultiAZStatus
    MultiAZStatus (..),

    -- ** NodeUpdateInitiatedBy
    NodeUpdateInitiatedBy (..),

    -- ** NodeUpdateStatus
    NodeUpdateStatus (..),

    -- ** OutpostMode
    OutpostMode (..),

    -- ** PendingAutomaticFailoverStatus
    PendingAutomaticFailoverStatus (..),

    -- ** ServiceUpdateSeverity
    ServiceUpdateSeverity (..),

    -- ** ServiceUpdateStatus
    ServiceUpdateStatus (..),

    -- ** ServiceUpdateType
    ServiceUpdateType (..),

    -- ** SlaMet
    SlaMet (..),

    -- ** SourceType
    SourceType (..),

    -- ** UpdateActionStatus
    UpdateActionStatus (..),

    -- ** Authentication
    Authentication (Authentication'),
    newAuthentication,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** CacheCluster
    CacheCluster (CacheCluster'),
    newCacheCluster,

    -- ** CacheEngineVersion
    CacheEngineVersion (CacheEngineVersion'),
    newCacheEngineVersion,

    -- ** CacheNode
    CacheNode (CacheNode'),
    newCacheNode,

    -- ** CacheNodeTypeSpecificParameter
    CacheNodeTypeSpecificParameter (CacheNodeTypeSpecificParameter'),
    newCacheNodeTypeSpecificParameter,

    -- ** CacheNodeTypeSpecificValue
    CacheNodeTypeSpecificValue (CacheNodeTypeSpecificValue'),
    newCacheNodeTypeSpecificValue,

    -- ** CacheNodeUpdateStatus
    CacheNodeUpdateStatus (CacheNodeUpdateStatus'),
    newCacheNodeUpdateStatus,

    -- ** CacheParameterGroup
    CacheParameterGroup (CacheParameterGroup'),
    newCacheParameterGroup,

    -- ** CacheParameterGroupNameMessage
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

    -- ** CacheParameterGroupStatus
    CacheParameterGroupStatus (CacheParameterGroupStatus'),
    newCacheParameterGroupStatus,

    -- ** CacheSecurityGroup
    CacheSecurityGroup (CacheSecurityGroup'),
    newCacheSecurityGroup,

    -- ** CacheSecurityGroupMembership
    CacheSecurityGroupMembership (CacheSecurityGroupMembership'),
    newCacheSecurityGroupMembership,

    -- ** CacheSubnetGroup
    CacheSubnetGroup (CacheSubnetGroup'),
    newCacheSubnetGroup,

    -- ** ConfigureShard
    ConfigureShard (ConfigureShard'),
    newConfigureShard,

    -- ** CustomerNodeEndpoint
    CustomerNodeEndpoint (CustomerNodeEndpoint'),
    newCustomerNodeEndpoint,

    -- ** EC2SecurityGroup
    EC2SecurityGroup (EC2SecurityGroup'),
    newEC2SecurityGroup,

    -- ** Endpoint
    Endpoint (Endpoint'),
    newEndpoint,

    -- ** EngineDefaults
    EngineDefaults (EngineDefaults'),
    newEngineDefaults,

    -- ** Event
    Event (Event'),
    newEvent,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** GlobalNodeGroup
    GlobalNodeGroup (GlobalNodeGroup'),
    newGlobalNodeGroup,

    -- ** GlobalReplicationGroup
    GlobalReplicationGroup (GlobalReplicationGroup'),
    newGlobalReplicationGroup,

    -- ** GlobalReplicationGroupInfo
    GlobalReplicationGroupInfo (GlobalReplicationGroupInfo'),
    newGlobalReplicationGroupInfo,

    -- ** GlobalReplicationGroupMember
    GlobalReplicationGroupMember (GlobalReplicationGroupMember'),
    newGlobalReplicationGroupMember,

    -- ** NodeGroup
    NodeGroup (NodeGroup'),
    newNodeGroup,

    -- ** NodeGroupConfiguration
    NodeGroupConfiguration (NodeGroupConfiguration'),
    newNodeGroupConfiguration,

    -- ** NodeGroupMember
    NodeGroupMember (NodeGroupMember'),
    newNodeGroupMember,

    -- ** NodeGroupMemberUpdateStatus
    NodeGroupMemberUpdateStatus (NodeGroupMemberUpdateStatus'),
    newNodeGroupMemberUpdateStatus,

    -- ** NodeGroupUpdateStatus
    NodeGroupUpdateStatus (NodeGroupUpdateStatus'),
    newNodeGroupUpdateStatus,

    -- ** NodeSnapshot
    NodeSnapshot (NodeSnapshot'),
    newNodeSnapshot,

    -- ** NotificationConfiguration
    NotificationConfiguration (NotificationConfiguration'),
    newNotificationConfiguration,

    -- ** Parameter
    Parameter (Parameter'),
    newParameter,

    -- ** ParameterNameValue
    ParameterNameValue (ParameterNameValue'),
    newParameterNameValue,

    -- ** PendingModifiedValues
    PendingModifiedValues (PendingModifiedValues'),
    newPendingModifiedValues,

    -- ** ProcessedUpdateAction
    ProcessedUpdateAction (ProcessedUpdateAction'),
    newProcessedUpdateAction,

    -- ** RecurringCharge
    RecurringCharge (RecurringCharge'),
    newRecurringCharge,

    -- ** RegionalConfiguration
    RegionalConfiguration (RegionalConfiguration'),
    newRegionalConfiguration,

    -- ** ReplicationGroup
    ReplicationGroup (ReplicationGroup'),
    newReplicationGroup,

    -- ** ReplicationGroupPendingModifiedValues
    ReplicationGroupPendingModifiedValues (ReplicationGroupPendingModifiedValues'),
    newReplicationGroupPendingModifiedValues,

    -- ** ReservedCacheNode
    ReservedCacheNode (ReservedCacheNode'),
    newReservedCacheNode,

    -- ** ReservedCacheNodesOffering
    ReservedCacheNodesOffering (ReservedCacheNodesOffering'),
    newReservedCacheNodesOffering,

    -- ** ReshardingConfiguration
    ReshardingConfiguration (ReshardingConfiguration'),
    newReshardingConfiguration,

    -- ** ReshardingStatus
    ReshardingStatus (ReshardingStatus'),
    newReshardingStatus,

    -- ** SecurityGroupMembership
    SecurityGroupMembership (SecurityGroupMembership'),
    newSecurityGroupMembership,

    -- ** ServiceUpdate
    ServiceUpdate (ServiceUpdate'),
    newServiceUpdate,

    -- ** SlotMigration
    SlotMigration (SlotMigration'),
    newSlotMigration,

    -- ** Snapshot
    Snapshot (Snapshot'),
    newSnapshot,

    -- ** Subnet
    Subnet (Subnet'),
    newSubnet,

    -- ** SubnetOutpost
    SubnetOutpost (SubnetOutpost'),
    newSubnetOutpost,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagListMessage
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** TimeRangeFilter
    TimeRangeFilter (TimeRangeFilter'),
    newTimeRangeFilter,

    -- ** UnprocessedUpdateAction
    UnprocessedUpdateAction (UnprocessedUpdateAction'),
    newUnprocessedUpdateAction,

    -- ** UpdateAction
    UpdateAction (UpdateAction'),
    newUpdateAction,

    -- ** UpdateActionResultsMessage
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

    -- ** User
    User (User'),
    newUser,

    -- ** UserGroup
    UserGroup (UserGroup'),
    newUserGroup,

    -- ** UserGroupPendingChanges
    UserGroupPendingChanges (UserGroupPendingChanges'),
    newUserGroupPendingChanges,

    -- ** UserGroupsUpdateStatus
    UserGroupsUpdateStatus (UserGroupsUpdateStatus'),
    newUserGroupsUpdateStatus,
  )
where

import Network.AWS.ElastiCache.AddTagsToResource
import Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.BatchApplyUpdateAction
import Network.AWS.ElastiCache.BatchStopUpdateAction
import Network.AWS.ElastiCache.CompleteMigration
import Network.AWS.ElastiCache.CopySnapshot
import Network.AWS.ElastiCache.CreateCacheCluster
import Network.AWS.ElastiCache.CreateCacheParameterGroup
import Network.AWS.ElastiCache.CreateCacheSecurityGroup
import Network.AWS.ElastiCache.CreateCacheSubnetGroup
import Network.AWS.ElastiCache.CreateGlobalReplicationGroup
import Network.AWS.ElastiCache.CreateReplicationGroup
import Network.AWS.ElastiCache.CreateSnapshot
import Network.AWS.ElastiCache.CreateUser
import Network.AWS.ElastiCache.CreateUserGroup
import Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
import Network.AWS.ElastiCache.DecreaseReplicaCount
import Network.AWS.ElastiCache.DeleteCacheCluster
import Network.AWS.ElastiCache.DeleteCacheParameterGroup
import Network.AWS.ElastiCache.DeleteCacheSecurityGroup
import Network.AWS.ElastiCache.DeleteCacheSubnetGroup
import Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
import Network.AWS.ElastiCache.DeleteReplicationGroup
import Network.AWS.ElastiCache.DeleteSnapshot
import Network.AWS.ElastiCache.DeleteUser
import Network.AWS.ElastiCache.DeleteUserGroup
import Network.AWS.ElastiCache.DescribeCacheClusters
import Network.AWS.ElastiCache.DescribeCacheEngineVersions
import Network.AWS.ElastiCache.DescribeCacheParameterGroups
import Network.AWS.ElastiCache.DescribeCacheParameters
import Network.AWS.ElastiCache.DescribeCacheSecurityGroups
import Network.AWS.ElastiCache.DescribeCacheSubnetGroups
import Network.AWS.ElastiCache.DescribeEngineDefaultParameters
import Network.AWS.ElastiCache.DescribeEvents
import Network.AWS.ElastiCache.DescribeGlobalReplicationGroups
import Network.AWS.ElastiCache.DescribeReplicationGroups
import Network.AWS.ElastiCache.DescribeReservedCacheNodes
import Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
import Network.AWS.ElastiCache.DescribeServiceUpdates
import Network.AWS.ElastiCache.DescribeSnapshots
import Network.AWS.ElastiCache.DescribeUpdateActions
import Network.AWS.ElastiCache.DescribeUserGroups
import Network.AWS.ElastiCache.DescribeUsers
import Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
import Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
import Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
import Network.AWS.ElastiCache.IncreaseReplicaCount
import Network.AWS.ElastiCache.Lens
import Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
import Network.AWS.ElastiCache.ListTagsForResource
import Network.AWS.ElastiCache.ModifyCacheCluster
import Network.AWS.ElastiCache.ModifyCacheParameterGroup
import Network.AWS.ElastiCache.ModifyCacheSubnetGroup
import Network.AWS.ElastiCache.ModifyGlobalReplicationGroup
import Network.AWS.ElastiCache.ModifyReplicationGroup
import Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
import Network.AWS.ElastiCache.ModifyUser
import Network.AWS.ElastiCache.ModifyUserGroup
import Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
import Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
import Network.AWS.ElastiCache.RebootCacheCluster
import Network.AWS.ElastiCache.RemoveTagsFromResource
import Network.AWS.ElastiCache.ResetCacheParameterGroup
import Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
import Network.AWS.ElastiCache.StartMigration
import Network.AWS.ElastiCache.TestFailover
import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ElastiCache'.

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
