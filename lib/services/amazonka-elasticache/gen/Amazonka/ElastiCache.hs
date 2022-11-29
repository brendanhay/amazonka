{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ElastiCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-02-02@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.ElastiCache
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** CacheParameterGroupAlreadyExistsFault
    _CacheParameterGroupAlreadyExistsFault,

    -- ** CacheSecurityGroupAlreadyExistsFault
    _CacheSecurityGroupAlreadyExistsFault,

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** UserGroupNotFoundFault
    _UserGroupNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** ReplicationGroupNotFoundFault
    _ReplicationGroupNotFoundFault,

    -- ** NodeGroupsPerReplicationGroupQuotaExceededFault
    _NodeGroupsPerReplicationGroupQuotaExceededFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** CacheSubnetGroupNotFoundFault
    _CacheSubnetGroupNotFoundFault,

    -- ** ServiceUpdateNotFoundFault
    _ServiceUpdateNotFoundFault,

    -- ** UserNotFoundFault
    _UserNotFoundFault,

    -- ** InvalidSnapshotStateFault
    _InvalidSnapshotStateFault,

    -- ** CacheClusterNotFoundFault
    _CacheClusterNotFoundFault,

    -- ** CacheSubnetGroupQuotaExceededFault
    _CacheSubnetGroupQuotaExceededFault,

    -- ** ReservedCacheNodeAlreadyExistsFault
    _ReservedCacheNodeAlreadyExistsFault,

    -- ** NoOperationFault
    _NoOperationFault,

    -- ** UserAlreadyExistsFault
    _UserAlreadyExistsFault,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** InvalidGlobalReplicationGroupStateFault
    _InvalidGlobalReplicationGroupStateFault,

    -- ** DefaultUserRequired
    _DefaultUserRequired,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** UserQuotaExceededFault
    _UserQuotaExceededFault,

    -- ** GlobalReplicationGroupNotFoundFault
    _GlobalReplicationGroupNotFoundFault,

    -- ** ReplicationGroupAlreadyExistsFault
    _ReplicationGroupAlreadyExistsFault,

    -- ** DuplicateUserNameFault
    _DuplicateUserNameFault,

    -- ** CacheParameterGroupQuotaExceededFault
    _CacheParameterGroupQuotaExceededFault,

    -- ** CacheSecurityGroupQuotaExceededFault
    _CacheSecurityGroupQuotaExceededFault,

    -- ** ReplicationGroupNotUnderMigrationFault
    _ReplicationGroupNotUnderMigrationFault,

    -- ** DefaultUserAssociatedToUserGroupFault
    _DefaultUserAssociatedToUserGroupFault,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** CacheParameterGroupNotFoundFault
    _CacheParameterGroupNotFoundFault,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** GlobalReplicationGroupAlreadyExistsFault
    _GlobalReplicationGroupAlreadyExistsFault,

    -- ** CacheSecurityGroupNotFoundFault
    _CacheSecurityGroupNotFoundFault,

    -- ** SnapshotAlreadyExistsFault
    _SnapshotAlreadyExistsFault,

    -- ** TestFailoverNotAvailableFault
    _TestFailoverNotAvailableFault,

    -- ** ReservedCacheNodesOfferingNotFoundFault
    _ReservedCacheNodesOfferingNotFoundFault,

    -- ** CacheSubnetGroupInUse
    _CacheSubnetGroupInUse,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** InvalidUserGroupStateFault
    _InvalidUserGroupStateFault,

    -- ** NodeGroupNotFoundFault
    _NodeGroupNotFoundFault,

    -- ** InvalidCacheSecurityGroupStateFault
    _InvalidCacheSecurityGroupStateFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** InsufficientCacheClusterCapacityFault
    _InsufficientCacheClusterCapacityFault,

    -- ** InvalidUserStateFault
    _InvalidUserStateFault,

    -- ** SnapshotNotFoundFault
    _SnapshotNotFoundFault,

    -- ** ReservedCacheNodeQuotaExceededFault
    _ReservedCacheNodeQuotaExceededFault,

    -- ** CacheSubnetGroupAlreadyExistsFault
    _CacheSubnetGroupAlreadyExistsFault,

    -- ** UserGroupAlreadyExistsFault
    _UserGroupAlreadyExistsFault,

    -- ** CacheSubnetQuotaExceededFault
    _CacheSubnetQuotaExceededFault,

    -- ** InvalidKMSKeyFault
    _InvalidKMSKeyFault,

    -- ** CacheClusterAlreadyExistsFault
    _CacheClusterAlreadyExistsFault,

    -- ** ReplicationGroupAlreadyUnderMigrationFault
    _ReplicationGroupAlreadyUnderMigrationFault,

    -- ** SnapshotFeatureNotSupportedFault
    _SnapshotFeatureNotSupportedFault,

    -- ** InvalidCacheClusterStateFault
    _InvalidCacheClusterStateFault,

    -- ** InvalidCacheParameterGroupStateFault
    _InvalidCacheParameterGroupStateFault,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** SubnetNotAllowedFault
    _SubnetNotAllowedFault,

    -- ** APICallRateForCustomerExceededFault
    _APICallRateForCustomerExceededFault,

    -- ** ReservedCacheNodeNotFoundFault
    _ReservedCacheNodeNotFoundFault,

    -- ** InvalidReplicationGroupStateFault
    _InvalidReplicationGroupStateFault,

    -- ** UserGroupQuotaExceededFault
    _UserGroupQuotaExceededFault,

    -- * Waiters
    -- $waiters

    -- ** ReplicationGroupAvailable
    newReplicationGroupAvailable,

    -- ** CacheClusterAvailable
    newCacheClusterAvailable,

    -- ** ReplicationGroupDeleted
    newReplicationGroupDeleted,

    -- ** CacheClusterDeleted
    newCacheClusterDeleted,

    -- * Operations
    -- $operations

    -- ** AddTagsToResource
    AddTagsToResource (AddTagsToResource'),
    newAddTagsToResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** AuthorizeCacheSecurityGroupIngress
    AuthorizeCacheSecurityGroupIngress (AuthorizeCacheSecurityGroupIngress'),
    newAuthorizeCacheSecurityGroupIngress,
    AuthorizeCacheSecurityGroupIngressResponse (AuthorizeCacheSecurityGroupIngressResponse'),
    newAuthorizeCacheSecurityGroupIngressResponse,

    -- ** BatchApplyUpdateAction
    BatchApplyUpdateAction (BatchApplyUpdateAction'),
    newBatchApplyUpdateAction,
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

    -- ** BatchStopUpdateAction
    BatchStopUpdateAction (BatchStopUpdateAction'),
    newBatchStopUpdateAction,
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

    -- ** CompleteMigration
    CompleteMigration (CompleteMigration'),
    newCompleteMigration,
    CompleteMigrationResponse (CompleteMigrationResponse'),
    newCompleteMigrationResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** CreateCacheCluster
    CreateCacheCluster (CreateCacheCluster'),
    newCreateCacheCluster,
    CreateCacheClusterResponse (CreateCacheClusterResponse'),
    newCreateCacheClusterResponse,

    -- ** CreateCacheParameterGroup
    CreateCacheParameterGroup (CreateCacheParameterGroup'),
    newCreateCacheParameterGroup,
    CreateCacheParameterGroupResponse (CreateCacheParameterGroupResponse'),
    newCreateCacheParameterGroupResponse,

    -- ** CreateCacheSecurityGroup
    CreateCacheSecurityGroup (CreateCacheSecurityGroup'),
    newCreateCacheSecurityGroup,
    CreateCacheSecurityGroupResponse (CreateCacheSecurityGroupResponse'),
    newCreateCacheSecurityGroupResponse,

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

    -- ** CreateReplicationGroup
    CreateReplicationGroup (CreateReplicationGroup'),
    newCreateReplicationGroup,
    CreateReplicationGroupResponse (CreateReplicationGroupResponse'),
    newCreateReplicationGroupResponse,

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    User (User'),
    newUser,

    -- ** CreateUserGroup
    CreateUserGroup (CreateUserGroup'),
    newCreateUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

    -- ** DecreaseNodeGroupsInGlobalReplicationGroup
    DecreaseNodeGroupsInGlobalReplicationGroup (DecreaseNodeGroupsInGlobalReplicationGroup'),
    newDecreaseNodeGroupsInGlobalReplicationGroup,
    DecreaseNodeGroupsInGlobalReplicationGroupResponse (DecreaseNodeGroupsInGlobalReplicationGroupResponse'),
    newDecreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** DecreaseReplicaCount
    DecreaseReplicaCount (DecreaseReplicaCount'),
    newDecreaseReplicaCount,
    DecreaseReplicaCountResponse (DecreaseReplicaCountResponse'),
    newDecreaseReplicaCountResponse,

    -- ** DeleteCacheCluster
    DeleteCacheCluster (DeleteCacheCluster'),
    newDeleteCacheCluster,
    DeleteCacheClusterResponse (DeleteCacheClusterResponse'),
    newDeleteCacheClusterResponse,

    -- ** DeleteCacheParameterGroup
    DeleteCacheParameterGroup (DeleteCacheParameterGroup'),
    newDeleteCacheParameterGroup,
    DeleteCacheParameterGroupResponse (DeleteCacheParameterGroupResponse'),
    newDeleteCacheParameterGroupResponse,

    -- ** DeleteCacheSecurityGroup
    DeleteCacheSecurityGroup (DeleteCacheSecurityGroup'),
    newDeleteCacheSecurityGroup,
    DeleteCacheSecurityGroupResponse (DeleteCacheSecurityGroupResponse'),
    newDeleteCacheSecurityGroupResponse,

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

    -- ** DeleteReplicationGroup
    DeleteReplicationGroup (DeleteReplicationGroup'),
    newDeleteReplicationGroup,
    DeleteReplicationGroupResponse (DeleteReplicationGroupResponse'),
    newDeleteReplicationGroupResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** DeleteUser
    DeleteUser (DeleteUser'),
    newDeleteUser,
    User (User'),
    newUser,

    -- ** DeleteUserGroup
    DeleteUserGroup (DeleteUserGroup'),
    newDeleteUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

    -- ** DescribeCacheClusters (Paginated)
    DescribeCacheClusters (DescribeCacheClusters'),
    newDescribeCacheClusters,
    DescribeCacheClustersResponse (DescribeCacheClustersResponse'),
    newDescribeCacheClustersResponse,

    -- ** DescribeCacheEngineVersions (Paginated)
    DescribeCacheEngineVersions (DescribeCacheEngineVersions'),
    newDescribeCacheEngineVersions,
    DescribeCacheEngineVersionsResponse (DescribeCacheEngineVersionsResponse'),
    newDescribeCacheEngineVersionsResponse,

    -- ** DescribeCacheParameterGroups (Paginated)
    DescribeCacheParameterGroups (DescribeCacheParameterGroups'),
    newDescribeCacheParameterGroups,
    DescribeCacheParameterGroupsResponse (DescribeCacheParameterGroupsResponse'),
    newDescribeCacheParameterGroupsResponse,

    -- ** DescribeCacheParameters (Paginated)
    DescribeCacheParameters (DescribeCacheParameters'),
    newDescribeCacheParameters,
    DescribeCacheParametersResponse (DescribeCacheParametersResponse'),
    newDescribeCacheParametersResponse,

    -- ** DescribeCacheSecurityGroups (Paginated)
    DescribeCacheSecurityGroups (DescribeCacheSecurityGroups'),
    newDescribeCacheSecurityGroups,
    DescribeCacheSecurityGroupsResponse (DescribeCacheSecurityGroupsResponse'),
    newDescribeCacheSecurityGroupsResponse,

    -- ** DescribeCacheSubnetGroups (Paginated)
    DescribeCacheSubnetGroups (DescribeCacheSubnetGroups'),
    newDescribeCacheSubnetGroups,
    DescribeCacheSubnetGroupsResponse (DescribeCacheSubnetGroupsResponse'),
    newDescribeCacheSubnetGroupsResponse,

    -- ** DescribeEngineDefaultParameters (Paginated)
    DescribeEngineDefaultParameters (DescribeEngineDefaultParameters'),
    newDescribeEngineDefaultParameters,
    DescribeEngineDefaultParametersResponse (DescribeEngineDefaultParametersResponse'),
    newDescribeEngineDefaultParametersResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeGlobalReplicationGroups (Paginated)
    DescribeGlobalReplicationGroups (DescribeGlobalReplicationGroups'),
    newDescribeGlobalReplicationGroups,
    DescribeGlobalReplicationGroupsResponse (DescribeGlobalReplicationGroupsResponse'),
    newDescribeGlobalReplicationGroupsResponse,

    -- ** DescribeReplicationGroups (Paginated)
    DescribeReplicationGroups (DescribeReplicationGroups'),
    newDescribeReplicationGroups,
    DescribeReplicationGroupsResponse (DescribeReplicationGroupsResponse'),
    newDescribeReplicationGroupsResponse,

    -- ** DescribeReservedCacheNodes (Paginated)
    DescribeReservedCacheNodes (DescribeReservedCacheNodes'),
    newDescribeReservedCacheNodes,
    DescribeReservedCacheNodesResponse (DescribeReservedCacheNodesResponse'),
    newDescribeReservedCacheNodesResponse,

    -- ** DescribeReservedCacheNodesOfferings (Paginated)
    DescribeReservedCacheNodesOfferings (DescribeReservedCacheNodesOfferings'),
    newDescribeReservedCacheNodesOfferings,
    DescribeReservedCacheNodesOfferingsResponse (DescribeReservedCacheNodesOfferingsResponse'),
    newDescribeReservedCacheNodesOfferingsResponse,

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

    -- ** DescribeUpdateActions (Paginated)
    DescribeUpdateActions (DescribeUpdateActions'),
    newDescribeUpdateActions,
    DescribeUpdateActionsResponse (DescribeUpdateActionsResponse'),
    newDescribeUpdateActionsResponse,

    -- ** DescribeUserGroups (Paginated)
    DescribeUserGroups (DescribeUserGroups'),
    newDescribeUserGroups,
    DescribeUserGroupsResponse (DescribeUserGroupsResponse'),
    newDescribeUserGroupsResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** DisassociateGlobalReplicationGroup
    DisassociateGlobalReplicationGroup (DisassociateGlobalReplicationGroup'),
    newDisassociateGlobalReplicationGroup,
    DisassociateGlobalReplicationGroupResponse (DisassociateGlobalReplicationGroupResponse'),
    newDisassociateGlobalReplicationGroupResponse,

    -- ** FailoverGlobalReplicationGroup
    FailoverGlobalReplicationGroup (FailoverGlobalReplicationGroup'),
    newFailoverGlobalReplicationGroup,
    FailoverGlobalReplicationGroupResponse (FailoverGlobalReplicationGroupResponse'),
    newFailoverGlobalReplicationGroupResponse,

    -- ** IncreaseNodeGroupsInGlobalReplicationGroup
    IncreaseNodeGroupsInGlobalReplicationGroup (IncreaseNodeGroupsInGlobalReplicationGroup'),
    newIncreaseNodeGroupsInGlobalReplicationGroup,
    IncreaseNodeGroupsInGlobalReplicationGroupResponse (IncreaseNodeGroupsInGlobalReplicationGroupResponse'),
    newIncreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** IncreaseReplicaCount
    IncreaseReplicaCount (IncreaseReplicaCount'),
    newIncreaseReplicaCount,
    IncreaseReplicaCountResponse (IncreaseReplicaCountResponse'),
    newIncreaseReplicaCountResponse,

    -- ** ListAllowedNodeTypeModifications
    ListAllowedNodeTypeModifications (ListAllowedNodeTypeModifications'),
    newListAllowedNodeTypeModifications,
    ListAllowedNodeTypeModificationsResponse (ListAllowedNodeTypeModificationsResponse'),
    newListAllowedNodeTypeModificationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** ModifyCacheCluster
    ModifyCacheCluster (ModifyCacheCluster'),
    newModifyCacheCluster,
    ModifyCacheClusterResponse (ModifyCacheClusterResponse'),
    newModifyCacheClusterResponse,

    -- ** ModifyCacheParameterGroup
    ModifyCacheParameterGroup (ModifyCacheParameterGroup'),
    newModifyCacheParameterGroup,
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

    -- ** ModifyCacheSubnetGroup
    ModifyCacheSubnetGroup (ModifyCacheSubnetGroup'),
    newModifyCacheSubnetGroup,
    ModifyCacheSubnetGroupResponse (ModifyCacheSubnetGroupResponse'),
    newModifyCacheSubnetGroupResponse,

    -- ** ModifyGlobalReplicationGroup
    ModifyGlobalReplicationGroup (ModifyGlobalReplicationGroup'),
    newModifyGlobalReplicationGroup,
    ModifyGlobalReplicationGroupResponse (ModifyGlobalReplicationGroupResponse'),
    newModifyGlobalReplicationGroupResponse,

    -- ** ModifyReplicationGroup
    ModifyReplicationGroup (ModifyReplicationGroup'),
    newModifyReplicationGroup,
    ModifyReplicationGroupResponse (ModifyReplicationGroupResponse'),
    newModifyReplicationGroupResponse,

    -- ** ModifyReplicationGroupShardConfiguration
    ModifyReplicationGroupShardConfiguration (ModifyReplicationGroupShardConfiguration'),
    newModifyReplicationGroupShardConfiguration,
    ModifyReplicationGroupShardConfigurationResponse (ModifyReplicationGroupShardConfigurationResponse'),
    newModifyReplicationGroupShardConfigurationResponse,

    -- ** ModifyUser
    ModifyUser (ModifyUser'),
    newModifyUser,
    User (User'),
    newUser,

    -- ** ModifyUserGroup
    ModifyUserGroup (ModifyUserGroup'),
    newModifyUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

    -- ** PurchaseReservedCacheNodesOffering
    PurchaseReservedCacheNodesOffering (PurchaseReservedCacheNodesOffering'),
    newPurchaseReservedCacheNodesOffering,
    PurchaseReservedCacheNodesOfferingResponse (PurchaseReservedCacheNodesOfferingResponse'),
    newPurchaseReservedCacheNodesOfferingResponse,

    -- ** RebalanceSlotsInGlobalReplicationGroup
    RebalanceSlotsInGlobalReplicationGroup (RebalanceSlotsInGlobalReplicationGroup'),
    newRebalanceSlotsInGlobalReplicationGroup,
    RebalanceSlotsInGlobalReplicationGroupResponse (RebalanceSlotsInGlobalReplicationGroupResponse'),
    newRebalanceSlotsInGlobalReplicationGroupResponse,

    -- ** RebootCacheCluster
    RebootCacheCluster (RebootCacheCluster'),
    newRebootCacheCluster,
    RebootCacheClusterResponse (RebootCacheClusterResponse'),
    newRebootCacheClusterResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** ResetCacheParameterGroup
    ResetCacheParameterGroup (ResetCacheParameterGroup'),
    newResetCacheParameterGroup,
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

    -- ** RevokeCacheSecurityGroupIngress
    RevokeCacheSecurityGroupIngress (RevokeCacheSecurityGroupIngress'),
    newRevokeCacheSecurityGroupIngress,
    RevokeCacheSecurityGroupIngressResponse (RevokeCacheSecurityGroupIngressResponse'),
    newRevokeCacheSecurityGroupIngressResponse,

    -- ** StartMigration
    StartMigration (StartMigration'),
    newStartMigration,
    StartMigrationResponse (StartMigrationResponse'),
    newStartMigrationResponse,

    -- ** TestFailover
    TestFailover (TestFailover'),
    newTestFailover,
    TestFailoverResponse (TestFailoverResponse'),
    newTestFailoverResponse,

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

    -- ** DataTieringStatus
    DataTieringStatus (..),

    -- ** DestinationType
    DestinationType (..),

    -- ** InputAuthenticationType
    InputAuthenticationType (..),

    -- ** IpDiscovery
    IpDiscovery (..),

    -- ** LogDeliveryConfigurationStatus
    LogDeliveryConfigurationStatus (..),

    -- ** LogFormat
    LogFormat (..),

    -- ** LogType
    LogType (..),

    -- ** MultiAZStatus
    MultiAZStatus (..),

    -- ** NetworkType
    NetworkType (..),

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

    -- ** AuthenticationMode
    AuthenticationMode (AuthenticationMode'),
    newAuthenticationMode,

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

    -- ** CloudWatchLogsDestinationDetails
    CloudWatchLogsDestinationDetails (CloudWatchLogsDestinationDetails'),
    newCloudWatchLogsDestinationDetails,

    -- ** ConfigureShard
    ConfigureShard (ConfigureShard'),
    newConfigureShard,

    -- ** CustomerNodeEndpoint
    CustomerNodeEndpoint (CustomerNodeEndpoint'),
    newCustomerNodeEndpoint,

    -- ** DestinationDetails
    DestinationDetails (DestinationDetails'),
    newDestinationDetails,

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

    -- ** KinesisFirehoseDestinationDetails
    KinesisFirehoseDestinationDetails (KinesisFirehoseDestinationDetails'),
    newKinesisFirehoseDestinationDetails,

    -- ** LogDeliveryConfiguration
    LogDeliveryConfiguration (LogDeliveryConfiguration'),
    newLogDeliveryConfiguration,

    -- ** LogDeliveryConfigurationRequest
    LogDeliveryConfigurationRequest (LogDeliveryConfigurationRequest'),
    newLogDeliveryConfigurationRequest,

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

    -- ** PendingLogDeliveryConfiguration
    PendingLogDeliveryConfiguration (PendingLogDeliveryConfiguration'),
    newPendingLogDeliveryConfiguration,

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

import Amazonka.ElastiCache.AddTagsToResource
import Amazonka.ElastiCache.AuthorizeCacheSecurityGroupIngress
import Amazonka.ElastiCache.BatchApplyUpdateAction
import Amazonka.ElastiCache.BatchStopUpdateAction
import Amazonka.ElastiCache.CompleteMigration
import Amazonka.ElastiCache.CopySnapshot
import Amazonka.ElastiCache.CreateCacheCluster
import Amazonka.ElastiCache.CreateCacheParameterGroup
import Amazonka.ElastiCache.CreateCacheSecurityGroup
import Amazonka.ElastiCache.CreateCacheSubnetGroup
import Amazonka.ElastiCache.CreateGlobalReplicationGroup
import Amazonka.ElastiCache.CreateReplicationGroup
import Amazonka.ElastiCache.CreateSnapshot
import Amazonka.ElastiCache.CreateUser
import Amazonka.ElastiCache.CreateUserGroup
import Amazonka.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
import Amazonka.ElastiCache.DecreaseReplicaCount
import Amazonka.ElastiCache.DeleteCacheCluster
import Amazonka.ElastiCache.DeleteCacheParameterGroup
import Amazonka.ElastiCache.DeleteCacheSecurityGroup
import Amazonka.ElastiCache.DeleteCacheSubnetGroup
import Amazonka.ElastiCache.DeleteGlobalReplicationGroup
import Amazonka.ElastiCache.DeleteReplicationGroup
import Amazonka.ElastiCache.DeleteSnapshot
import Amazonka.ElastiCache.DeleteUser
import Amazonka.ElastiCache.DeleteUserGroup
import Amazonka.ElastiCache.DescribeCacheClusters
import Amazonka.ElastiCache.DescribeCacheEngineVersions
import Amazonka.ElastiCache.DescribeCacheParameterGroups
import Amazonka.ElastiCache.DescribeCacheParameters
import Amazonka.ElastiCache.DescribeCacheSecurityGroups
import Amazonka.ElastiCache.DescribeCacheSubnetGroups
import Amazonka.ElastiCache.DescribeEngineDefaultParameters
import Amazonka.ElastiCache.DescribeEvents
import Amazonka.ElastiCache.DescribeGlobalReplicationGroups
import Amazonka.ElastiCache.DescribeReplicationGroups
import Amazonka.ElastiCache.DescribeReservedCacheNodes
import Amazonka.ElastiCache.DescribeReservedCacheNodesOfferings
import Amazonka.ElastiCache.DescribeServiceUpdates
import Amazonka.ElastiCache.DescribeSnapshots
import Amazonka.ElastiCache.DescribeUpdateActions
import Amazonka.ElastiCache.DescribeUserGroups
import Amazonka.ElastiCache.DescribeUsers
import Amazonka.ElastiCache.DisassociateGlobalReplicationGroup
import Amazonka.ElastiCache.FailoverGlobalReplicationGroup
import Amazonka.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
import Amazonka.ElastiCache.IncreaseReplicaCount
import Amazonka.ElastiCache.Lens
import Amazonka.ElastiCache.ListAllowedNodeTypeModifications
import Amazonka.ElastiCache.ListTagsForResource
import Amazonka.ElastiCache.ModifyCacheCluster
import Amazonka.ElastiCache.ModifyCacheParameterGroup
import Amazonka.ElastiCache.ModifyCacheSubnetGroup
import Amazonka.ElastiCache.ModifyGlobalReplicationGroup
import Amazonka.ElastiCache.ModifyReplicationGroup
import Amazonka.ElastiCache.ModifyReplicationGroupShardConfiguration
import Amazonka.ElastiCache.ModifyUser
import Amazonka.ElastiCache.ModifyUserGroup
import Amazonka.ElastiCache.PurchaseReservedCacheNodesOffering
import Amazonka.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
import Amazonka.ElastiCache.RebootCacheCluster
import Amazonka.ElastiCache.RemoveTagsFromResource
import Amazonka.ElastiCache.ResetCacheParameterGroup
import Amazonka.ElastiCache.RevokeCacheSecurityGroupIngress
import Amazonka.ElastiCache.StartMigration
import Amazonka.ElastiCache.TestFailover
import Amazonka.ElastiCache.Types
import Amazonka.ElastiCache.Waiters

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
