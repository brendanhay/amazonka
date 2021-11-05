{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ElastiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
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

    -- ** CacheSubnetGroupInUse
    _CacheSubnetGroupInUse,

    -- ** ReservedCacheNodeAlreadyExistsFault
    _ReservedCacheNodeAlreadyExistsFault,

    -- ** CacheSecurityGroupNotFoundFault
    _CacheSecurityGroupNotFoundFault,

    -- ** InvalidGlobalReplicationGroupStateFault
    _InvalidGlobalReplicationGroupStateFault,

    -- ** CacheSubnetGroupAlreadyExistsFault
    _CacheSubnetGroupAlreadyExistsFault,

    -- ** GlobalReplicationGroupAlreadyExistsFault
    _GlobalReplicationGroupAlreadyExistsFault,

    -- ** NodeGroupsPerReplicationGroupQuotaExceededFault
    _NodeGroupsPerReplicationGroupQuotaExceededFault,

    -- ** CacheSubnetGroupQuotaExceededFault
    _CacheSubnetGroupQuotaExceededFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** ReservedCacheNodeQuotaExceededFault
    _ReservedCacheNodeQuotaExceededFault,

    -- ** ReservedCacheNodesOfferingNotFoundFault
    _ReservedCacheNodesOfferingNotFoundFault,

    -- ** ReplicationGroupNotFoundFault
    _ReplicationGroupNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

    -- ** InvalidUserGroupStateFault
    _InvalidUserGroupStateFault,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** UserAlreadyExistsFault
    _UserAlreadyExistsFault,

    -- ** InvalidUserStateFault
    _InvalidUserStateFault,

    -- ** SnapshotNotFoundFault
    _SnapshotNotFoundFault,

    -- ** InsufficientCacheClusterCapacityFault
    _InsufficientCacheClusterCapacityFault,

    -- ** InvalidSnapshotStateFault
    _InvalidSnapshotStateFault,

    -- ** SnapshotAlreadyExistsFault
    _SnapshotAlreadyExistsFault,

    -- ** DefaultUserRequired
    _DefaultUserRequired,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** APICallRateForCustomerExceededFault
    _APICallRateForCustomerExceededFault,

    -- ** NodeGroupNotFoundFault
    _NodeGroupNotFoundFault,

    -- ** CacheParameterGroupAlreadyExistsFault
    _CacheParameterGroupAlreadyExistsFault,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** InvalidKMSKeyFault
    _InvalidKMSKeyFault,

    -- ** GlobalReplicationGroupNotFoundFault
    _GlobalReplicationGroupNotFoundFault,

    -- ** ReservedCacheNodeNotFoundFault
    _ReservedCacheNodeNotFoundFault,

    -- ** CacheSubnetGroupNotFoundFault
    _CacheSubnetGroupNotFoundFault,

    -- ** SnapshotFeatureNotSupportedFault
    _SnapshotFeatureNotSupportedFault,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** TestFailoverNotAvailableFault
    _TestFailoverNotAvailableFault,

    -- ** SubnetNotAllowedFault
    _SubnetNotAllowedFault,

    -- ** InvalidReplicationGroupStateFault
    _InvalidReplicationGroupStateFault,

    -- ** ReplicationGroupAlreadyExistsFault
    _ReplicationGroupAlreadyExistsFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** UserGroupNotFoundFault
    _UserGroupNotFoundFault,

    -- ** CacheClusterNotFoundFault
    _CacheClusterNotFoundFault,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** UserGroupAlreadyExistsFault
    _UserGroupAlreadyExistsFault,

    -- ** InvalidCacheClusterStateFault
    _InvalidCacheClusterStateFault,

    -- ** CacheSecurityGroupQuotaExceededFault
    _CacheSecurityGroupQuotaExceededFault,

    -- ** CacheClusterAlreadyExistsFault
    _CacheClusterAlreadyExistsFault,

    -- ** CacheParameterGroupQuotaExceededFault
    _CacheParameterGroupQuotaExceededFault,

    -- ** ServiceUpdateNotFoundFault
    _ServiceUpdateNotFoundFault,

    -- ** DefaultUserAssociatedToUserGroupFault
    _DefaultUserAssociatedToUserGroupFault,

    -- ** UserNotFoundFault
    _UserNotFoundFault,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** CacheSubnetQuotaExceededFault
    _CacheSubnetQuotaExceededFault,

    -- ** ReplicationGroupNotUnderMigrationFault
    _ReplicationGroupNotUnderMigrationFault,

    -- ** ReplicationGroupAlreadyUnderMigrationFault
    _ReplicationGroupAlreadyUnderMigrationFault,

    -- ** CacheParameterGroupNotFoundFault
    _CacheParameterGroupNotFoundFault,

    -- ** DuplicateUserNameFault
    _DuplicateUserNameFault,

    -- ** UserQuotaExceededFault
    _UserQuotaExceededFault,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** NoOperationFault
    _NoOperationFault,

    -- ** InvalidCacheParameterGroupStateFault
    _InvalidCacheParameterGroupStateFault,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** UserGroupQuotaExceededFault
    _UserGroupQuotaExceededFault,

    -- ** InvalidCacheSecurityGroupStateFault
    _InvalidCacheSecurityGroupStateFault,

    -- ** CacheSecurityGroupAlreadyExistsFault
    _CacheSecurityGroupAlreadyExistsFault,

    -- * Waiters
    -- $waiters

    -- ** CacheClusterAvailable
    newCacheClusterAvailable,

    -- ** CacheClusterDeleted
    newCacheClusterDeleted,

    -- ** ReplicationGroupDeleted
    newReplicationGroupDeleted,

    -- ** ReplicationGroupAvailable
    newReplicationGroupAvailable,

    -- * Operations
    -- $operations

    -- ** StartMigration
    StartMigration (StartMigration'),
    newStartMigration,
    StartMigrationResponse (StartMigrationResponse'),
    newStartMigrationResponse,

    -- ** DeleteCacheSecurityGroup
    DeleteCacheSecurityGroup (DeleteCacheSecurityGroup'),
    newDeleteCacheSecurityGroup,
    DeleteCacheSecurityGroupResponse (DeleteCacheSecurityGroupResponse'),
    newDeleteCacheSecurityGroupResponse,

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

    -- ** IncreaseNodeGroupsInGlobalReplicationGroup
    IncreaseNodeGroupsInGlobalReplicationGroup (IncreaseNodeGroupsInGlobalReplicationGroup'),
    newIncreaseNodeGroupsInGlobalReplicationGroup,
    IncreaseNodeGroupsInGlobalReplicationGroupResponse (IncreaseNodeGroupsInGlobalReplicationGroupResponse'),
    newIncreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- ** DescribeUsers (Paginated)
    DescribeUsers (DescribeUsers'),
    newDescribeUsers,
    DescribeUsersResponse (DescribeUsersResponse'),
    newDescribeUsersResponse,

    -- ** RebootCacheCluster
    RebootCacheCluster (RebootCacheCluster'),
    newRebootCacheCluster,
    RebootCacheClusterResponse (RebootCacheClusterResponse'),
    newRebootCacheClusterResponse,

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

    -- ** CreateCacheCluster
    CreateCacheCluster (CreateCacheCluster'),
    newCreateCacheCluster,
    CreateCacheClusterResponse (CreateCacheClusterResponse'),
    newCreateCacheClusterResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** DescribeEngineDefaultParameters (Paginated)
    DescribeEngineDefaultParameters (DescribeEngineDefaultParameters'),
    newDescribeEngineDefaultParameters,
    DescribeEngineDefaultParametersResponse (DescribeEngineDefaultParametersResponse'),
    newDescribeEngineDefaultParametersResponse,

    -- ** DisassociateGlobalReplicationGroup
    DisassociateGlobalReplicationGroup (DisassociateGlobalReplicationGroup'),
    newDisassociateGlobalReplicationGroup,
    DisassociateGlobalReplicationGroupResponse (DisassociateGlobalReplicationGroupResponse'),
    newDisassociateGlobalReplicationGroupResponse,

    -- ** ModifyCacheParameterGroup
    ModifyCacheParameterGroup (ModifyCacheParameterGroup'),
    newModifyCacheParameterGroup,
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

    -- ** TestFailover
    TestFailover (TestFailover'),
    newTestFailover,
    TestFailoverResponse (TestFailoverResponse'),
    newTestFailoverResponse,

    -- ** DeleteReplicationGroup
    DeleteReplicationGroup (DeleteReplicationGroup'),
    newDeleteReplicationGroup,
    DeleteReplicationGroupResponse (DeleteReplicationGroupResponse'),
    newDeleteReplicationGroupResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** CompleteMigration
    CompleteMigration (CompleteMigration'),
    newCompleteMigration,
    CompleteMigrationResponse (CompleteMigrationResponse'),
    newCompleteMigrationResponse,

    -- ** DescribeCacheClusters (Paginated)
    DescribeCacheClusters (DescribeCacheClusters'),
    newDescribeCacheClusters,
    DescribeCacheClustersResponse (DescribeCacheClustersResponse'),
    newDescribeCacheClustersResponse,

    -- ** PurchaseReservedCacheNodesOffering
    PurchaseReservedCacheNodesOffering (PurchaseReservedCacheNodesOffering'),
    newPurchaseReservedCacheNodesOffering,
    PurchaseReservedCacheNodesOfferingResponse (PurchaseReservedCacheNodesOfferingResponse'),
    newPurchaseReservedCacheNodesOfferingResponse,

    -- ** RemoveTagsFromResource
    RemoveTagsFromResource (RemoveTagsFromResource'),
    newRemoveTagsFromResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** ModifyReplicationGroup
    ModifyReplicationGroup (ModifyReplicationGroup'),
    newModifyReplicationGroup,
    ModifyReplicationGroupResponse (ModifyReplicationGroupResponse'),
    newModifyReplicationGroupResponse,

    -- ** DescribeCacheParameters (Paginated)
    DescribeCacheParameters (DescribeCacheParameters'),
    newDescribeCacheParameters,
    DescribeCacheParametersResponse (DescribeCacheParametersResponse'),
    newDescribeCacheParametersResponse,

    -- ** DescribeGlobalReplicationGroups (Paginated)
    DescribeGlobalReplicationGroups (DescribeGlobalReplicationGroups'),
    newDescribeGlobalReplicationGroups,
    DescribeGlobalReplicationGroupsResponse (DescribeGlobalReplicationGroupsResponse'),
    newDescribeGlobalReplicationGroupsResponse,

    -- ** DescribeCacheSubnetGroups (Paginated)
    DescribeCacheSubnetGroups (DescribeCacheSubnetGroups'),
    newDescribeCacheSubnetGroups,
    DescribeCacheSubnetGroupsResponse (DescribeCacheSubnetGroupsResponse'),
    newDescribeCacheSubnetGroupsResponse,

    -- ** DescribeUpdateActions (Paginated)
    DescribeUpdateActions (DescribeUpdateActions'),
    newDescribeUpdateActions,
    DescribeUpdateActionsResponse (DescribeUpdateActionsResponse'),
    newDescribeUpdateActionsResponse,

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

    -- ** DecreaseReplicaCount
    DecreaseReplicaCount (DecreaseReplicaCount'),
    newDecreaseReplicaCount,
    DecreaseReplicaCountResponse (DecreaseReplicaCountResponse'),
    newDecreaseReplicaCountResponse,

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

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** FailoverGlobalReplicationGroup
    FailoverGlobalReplicationGroup (FailoverGlobalReplicationGroup'),
    newFailoverGlobalReplicationGroup,
    FailoverGlobalReplicationGroupResponse (FailoverGlobalReplicationGroupResponse'),
    newFailoverGlobalReplicationGroupResponse,

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

    -- ** ResetCacheParameterGroup
    ResetCacheParameterGroup (ResetCacheParameterGroup'),
    newResetCacheParameterGroup,
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

    -- ** ListAllowedNodeTypeModifications
    ListAllowedNodeTypeModifications (ListAllowedNodeTypeModifications'),
    newListAllowedNodeTypeModifications,
    ListAllowedNodeTypeModificationsResponse (ListAllowedNodeTypeModificationsResponse'),
    newListAllowedNodeTypeModificationsResponse,

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

    -- ** BatchApplyUpdateAction
    BatchApplyUpdateAction (BatchApplyUpdateAction'),
    newBatchApplyUpdateAction,
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

    -- ** DeleteUserGroup
    DeleteUserGroup (DeleteUserGroup'),
    newDeleteUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

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

    -- ** DescribeReplicationGroups (Paginated)
    DescribeReplicationGroups (DescribeReplicationGroups'),
    newDescribeReplicationGroups,
    DescribeReplicationGroupsResponse (DescribeReplicationGroupsResponse'),
    newDescribeReplicationGroupsResponse,

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

    -- ** DescribeReservedCacheNodesOfferings (Paginated)
    DescribeReservedCacheNodesOfferings (DescribeReservedCacheNodesOfferings'),
    newDescribeReservedCacheNodesOfferings,
    DescribeReservedCacheNodesOfferingsResponse (DescribeReservedCacheNodesOfferingsResponse'),
    newDescribeReservedCacheNodesOfferingsResponse,

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

    -- ** CreateSnapshot
    CreateSnapshot (CreateSnapshot'),
    newCreateSnapshot,
    CreateSnapshotResponse (CreateSnapshotResponse'),
    newCreateSnapshotResponse,

    -- ** ModifyGlobalReplicationGroup
    ModifyGlobalReplicationGroup (ModifyGlobalReplicationGroup'),
    newModifyGlobalReplicationGroup,
    ModifyGlobalReplicationGroupResponse (ModifyGlobalReplicationGroupResponse'),
    newModifyGlobalReplicationGroupResponse,

    -- ** DescribeUserGroups (Paginated)
    DescribeUserGroups (DescribeUserGroups'),
    newDescribeUserGroups,
    DescribeUserGroupsResponse (DescribeUserGroupsResponse'),
    newDescribeUserGroupsResponse,

    -- ** DeleteCacheParameterGroup
    DeleteCacheParameterGroup (DeleteCacheParameterGroup'),
    newDeleteCacheParameterGroup,
    DeleteCacheParameterGroupResponse (DeleteCacheParameterGroupResponse'),
    newDeleteCacheParameterGroupResponse,

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

    -- ** ModifyCacheCluster
    ModifyCacheCluster (ModifyCacheCluster'),
    newModifyCacheCluster,
    ModifyCacheClusterResponse (ModifyCacheClusterResponse'),
    newModifyCacheClusterResponse,

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

    -- ** CreateCacheParameterGroup
    CreateCacheParameterGroup (CreateCacheParameterGroup'),
    newCreateCacheParameterGroup,
    CreateCacheParameterGroupResponse (CreateCacheParameterGroupResponse'),
    newCreateCacheParameterGroupResponse,

    -- ** DescribeReservedCacheNodes (Paginated)
    DescribeReservedCacheNodes (DescribeReservedCacheNodes'),
    newDescribeReservedCacheNodes,
    DescribeReservedCacheNodesResponse (DescribeReservedCacheNodesResponse'),
    newDescribeReservedCacheNodesResponse,

    -- ** DeleteGlobalReplicationGroup
    DeleteGlobalReplicationGroup (DeleteGlobalReplicationGroup'),
    newDeleteGlobalReplicationGroup,
    DeleteGlobalReplicationGroupResponse (DeleteGlobalReplicationGroupResponse'),
    newDeleteGlobalReplicationGroupResponse,

    -- ** DecreaseNodeGroupsInGlobalReplicationGroup
    DecreaseNodeGroupsInGlobalReplicationGroup (DecreaseNodeGroupsInGlobalReplicationGroup'),
    newDecreaseNodeGroupsInGlobalReplicationGroup,
    DecreaseNodeGroupsInGlobalReplicationGroupResponse (DecreaseNodeGroupsInGlobalReplicationGroupResponse'),
    newDecreaseNodeGroupsInGlobalReplicationGroupResponse,

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

    -- ** DestinationType
    DestinationType (..),

    -- ** LogDeliveryConfigurationStatus
    LogDeliveryConfigurationStatus (..),

    -- ** LogFormat
    LogFormat (..),

    -- ** LogType
    LogType (..),

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
