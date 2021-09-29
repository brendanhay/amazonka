{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.ElastiCache
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
module Network.AWS.ElastiCache
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** SubnetInUse
    _SubnetInUse,

    -- ** CacheClusterNotFoundFault
    _CacheClusterNotFoundFault,

    -- ** InvalidReplicationGroupStateFault
    _InvalidReplicationGroupStateFault,

    -- ** InvalidVPCNetworkStateFault
    _InvalidVPCNetworkStateFault,

    -- ** CacheSubnetGroupQuotaExceededFault
    _CacheSubnetGroupQuotaExceededFault,

    -- ** CacheSubnetGroupAlreadyExistsFault
    _CacheSubnetGroupAlreadyExistsFault,

    -- ** SubnetNotAllowedFault
    _SubnetNotAllowedFault,

    -- ** GlobalReplicationGroupAlreadyExistsFault
    _GlobalReplicationGroupAlreadyExistsFault,

    -- ** TestFailoverNotAvailableFault
    _TestFailoverNotAvailableFault,

    -- ** InvalidKMSKeyFault
    _InvalidKMSKeyFault,

    -- ** UserGroupQuotaExceededFault
    _UserGroupQuotaExceededFault,

    -- ** SnapshotFeatureNotSupportedFault
    _SnapshotFeatureNotSupportedFault,

    -- ** InvalidCacheSecurityGroupStateFault
    _InvalidCacheSecurityGroupStateFault,

    -- ** ReservedCacheNodeNotFoundFault
    _ReservedCacheNodeNotFoundFault,

    -- ** ServiceLinkedRoleNotFoundFault
    _ServiceLinkedRoleNotFoundFault,

    -- ** UserQuotaExceededFault
    _UserQuotaExceededFault,

    -- ** APICallRateForCustomerExceededFault
    _APICallRateForCustomerExceededFault,

    -- ** InvalidParameterCombinationException
    _InvalidParameterCombinationException,

    -- ** InvalidCacheParameterGroupStateFault
    _InvalidCacheParameterGroupStateFault,

    -- ** CacheParameterGroupAlreadyExistsFault
    _CacheParameterGroupAlreadyExistsFault,

    -- ** NoOperationFault
    _NoOperationFault,

    -- ** SnapshotQuotaExceededFault
    _SnapshotQuotaExceededFault,

    -- ** TagNotFoundFault
    _TagNotFoundFault,

    -- ** SnapshotAlreadyExistsFault
    _SnapshotAlreadyExistsFault,

    -- ** UserNotFoundFault
    _UserNotFoundFault,

    -- ** NodeQuotaForCustomerExceededFault
    _NodeQuotaForCustomerExceededFault,

    -- ** CacheSubnetQuotaExceededFault
    _CacheSubnetQuotaExceededFault,

    -- ** ServiceUpdateNotFoundFault
    _ServiceUpdateNotFoundFault,

    -- ** SnapshotNotFoundFault
    _SnapshotNotFoundFault,

    -- ** InsufficientCacheClusterCapacityFault
    _InsufficientCacheClusterCapacityFault,

    -- ** InvalidUserStateFault
    _InvalidUserStateFault,

    -- ** InvalidCacheClusterStateFault
    _InvalidCacheClusterStateFault,

    -- ** UserGroupAlreadyExistsFault
    _UserGroupAlreadyExistsFault,

    -- ** InvalidUserGroupStateFault
    _InvalidUserGroupStateFault,

    -- ** ReplicationGroupAlreadyExistsFault
    _ReplicationGroupAlreadyExistsFault,

    -- ** ReservedCacheNodeQuotaExceededFault
    _ReservedCacheNodeQuotaExceededFault,

    -- ** AuthorizationAlreadyExistsFault
    _AuthorizationAlreadyExistsFault,

    -- ** UserGroupNotFoundFault
    _UserGroupNotFoundFault,

    -- ** NodeGroupsPerReplicationGroupQuotaExceededFault
    _NodeGroupsPerReplicationGroupQuotaExceededFault,

    -- ** InvalidGlobalReplicationGroupStateFault
    _InvalidGlobalReplicationGroupStateFault,

    -- ** ReservedCacheNodeAlreadyExistsFault
    _ReservedCacheNodeAlreadyExistsFault,

    -- ** CacheSubnetGroupInUse
    _CacheSubnetGroupInUse,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** CacheSecurityGroupNotFoundFault
    _CacheSecurityGroupNotFoundFault,

    -- ** GlobalReplicationGroupNotFoundFault
    _GlobalReplicationGroupNotFoundFault,

    -- ** CacheSecurityGroupAlreadyExistsFault
    _CacheSecurityGroupAlreadyExistsFault,

    -- ** CacheSubnetGroupNotFoundFault
    _CacheSubnetGroupNotFoundFault,

    -- ** NodeQuotaForClusterExceededFault
    _NodeQuotaForClusterExceededFault,

    -- ** NodeGroupNotFoundFault
    _NodeGroupNotFoundFault,

    -- ** InvalidARNFault
    _InvalidARNFault,

    -- ** CacheParameterGroupNotFoundFault
    _CacheParameterGroupNotFoundFault,

    -- ** DuplicateUserNameFault
    _DuplicateUserNameFault,

    -- ** DefaultUserRequired
    _DefaultUserRequired,

    -- ** ReplicationGroupAlreadyUnderMigrationFault
    _ReplicationGroupAlreadyUnderMigrationFault,

    -- ** ReplicationGroupNotUnderMigrationFault
    _ReplicationGroupNotUnderMigrationFault,

    -- ** InvalidSnapshotStateFault
    _InvalidSnapshotStateFault,

    -- ** UserAlreadyExistsFault
    _UserAlreadyExistsFault,

    -- ** CacheParameterGroupQuotaExceededFault
    _CacheParameterGroupQuotaExceededFault,

    -- ** DefaultUserAssociatedToUserGroupFault
    _DefaultUserAssociatedToUserGroupFault,

    -- ** CacheClusterAlreadyExistsFault
    _CacheClusterAlreadyExistsFault,

    -- ** ReservedCacheNodesOfferingNotFoundFault
    _ReservedCacheNodesOfferingNotFoundFault,

    -- ** TagQuotaPerResourceExceeded
    _TagQuotaPerResourceExceeded,

    -- ** AuthorizationNotFoundFault
    _AuthorizationNotFoundFault,

    -- ** ClusterQuotaForCustomerExceededFault
    _ClusterQuotaForCustomerExceededFault,

    -- ** ReplicationGroupNotFoundFault
    _ReplicationGroupNotFoundFault,

    -- ** InvalidSubnet
    _InvalidSubnet,

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

    -- ** DeleteUserGroup
    DeleteUserGroup (DeleteUserGroup'),
    newDeleteUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

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

    -- ** AuthorizeCacheSecurityGroupIngress
    AuthorizeCacheSecurityGroupIngress (AuthorizeCacheSecurityGroupIngress'),
    newAuthorizeCacheSecurityGroupIngress,
    AuthorizeCacheSecurityGroupIngressResponse (AuthorizeCacheSecurityGroupIngressResponse'),
    newAuthorizeCacheSecurityGroupIngressResponse,

    -- ** DecreaseReplicaCount
    DecreaseReplicaCount (DecreaseReplicaCount'),
    newDecreaseReplicaCount,
    DecreaseReplicaCountResponse (DecreaseReplicaCountResponse'),
    newDecreaseReplicaCountResponse,

    -- ** CreateCacheSecurityGroup
    CreateCacheSecurityGroup (CreateCacheSecurityGroup'),
    newCreateCacheSecurityGroup,
    CreateCacheSecurityGroupResponse (CreateCacheSecurityGroupResponse'),
    newCreateCacheSecurityGroupResponse,

    -- ** RebalanceSlotsInGlobalReplicationGroup
    RebalanceSlotsInGlobalReplicationGroup (RebalanceSlotsInGlobalReplicationGroup'),
    newRebalanceSlotsInGlobalReplicationGroup,
    RebalanceSlotsInGlobalReplicationGroupResponse (RebalanceSlotsInGlobalReplicationGroupResponse'),
    newRebalanceSlotsInGlobalReplicationGroupResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** ModifyCacheCluster
    ModifyCacheCluster (ModifyCacheCluster'),
    newModifyCacheCluster,
    ModifyCacheClusterResponse (ModifyCacheClusterResponse'),
    newModifyCacheClusterResponse,

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

    -- ** ModifyReplicationGroup
    ModifyReplicationGroup (ModifyReplicationGroup'),
    newModifyReplicationGroup,
    ModifyReplicationGroupResponse (ModifyReplicationGroupResponse'),
    newModifyReplicationGroupResponse,

    -- ** BatchStopUpdateAction
    BatchStopUpdateAction (BatchStopUpdateAction'),
    newBatchStopUpdateAction,
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

    -- ** PurchaseReservedCacheNodesOffering
    PurchaseReservedCacheNodesOffering (PurchaseReservedCacheNodesOffering'),
    newPurchaseReservedCacheNodesOffering,
    PurchaseReservedCacheNodesOfferingResponse (PurchaseReservedCacheNodesOfferingResponse'),
    newPurchaseReservedCacheNodesOfferingResponse,

    -- ** DeleteSnapshot
    DeleteSnapshot (DeleteSnapshot'),
    newDeleteSnapshot,
    DeleteSnapshotResponse (DeleteSnapshotResponse'),
    newDeleteSnapshotResponse,

    -- ** CreateUser
    CreateUser (CreateUser'),
    newCreateUser,
    User (User'),
    newUser,

    -- ** CompleteMigration
    CompleteMigration (CompleteMigration'),
    newCompleteMigration,
    CompleteMigrationResponse (CompleteMigrationResponse'),
    newCompleteMigrationResponse,

    -- ** DescribeEvents (Paginated)
    DescribeEvents (DescribeEvents'),
    newDescribeEvents,
    DescribeEventsResponse (DescribeEventsResponse'),
    newDescribeEventsResponse,

    -- ** TestFailover
    TestFailover (TestFailover'),
    newTestFailover,
    TestFailoverResponse (TestFailoverResponse'),
    newTestFailoverResponse,

    -- ** DescribeSnapshots (Paginated)
    DescribeSnapshots (DescribeSnapshots'),
    newDescribeSnapshots,
    DescribeSnapshotsResponse (DescribeSnapshotsResponse'),
    newDescribeSnapshotsResponse,

    -- ** DisassociateGlobalReplicationGroup
    DisassociateGlobalReplicationGroup (DisassociateGlobalReplicationGroup'),
    newDisassociateGlobalReplicationGroup,
    DisassociateGlobalReplicationGroupResponse (DisassociateGlobalReplicationGroupResponse'),
    newDisassociateGlobalReplicationGroupResponse,

    -- ** DeleteReplicationGroup
    DeleteReplicationGroup (DeleteReplicationGroup'),
    newDeleteReplicationGroup,
    DeleteReplicationGroupResponse (DeleteReplicationGroupResponse'),
    newDeleteReplicationGroupResponse,

    -- ** CreateCacheCluster
    CreateCacheCluster (CreateCacheCluster'),
    newCreateCacheCluster,
    CreateCacheClusterResponse (CreateCacheClusterResponse'),
    newCreateCacheClusterResponse,

    -- ** BatchApplyUpdateAction
    BatchApplyUpdateAction (BatchApplyUpdateAction'),
    newBatchApplyUpdateAction,
    UpdateActionResultsMessage (UpdateActionResultsMessage'),
    newUpdateActionResultsMessage,

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

    -- ** ResetCacheParameterGroup
    ResetCacheParameterGroup (ResetCacheParameterGroup'),
    newResetCacheParameterGroup,
    CacheParameterGroupNameMessage (CacheParameterGroupNameMessage'),
    newCacheParameterGroupNameMessage,

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

    -- ** DeleteGlobalReplicationGroup
    DeleteGlobalReplicationGroup (DeleteGlobalReplicationGroup'),
    newDeleteGlobalReplicationGroup,
    DeleteGlobalReplicationGroupResponse (DeleteGlobalReplicationGroupResponse'),
    newDeleteGlobalReplicationGroupResponse,

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

    -- ** DescribeUpdateActions (Paginated)
    DescribeUpdateActions (DescribeUpdateActions'),
    newDescribeUpdateActions,
    DescribeUpdateActionsResponse (DescribeUpdateActionsResponse'),
    newDescribeUpdateActionsResponse,

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

    -- ** DescribeCacheParameters (Paginated)
    DescribeCacheParameters (DescribeCacheParameters'),
    newDescribeCacheParameters,
    DescribeCacheParametersResponse (DescribeCacheParametersResponse'),
    newDescribeCacheParametersResponse,

    -- ** DescribeUserGroups (Paginated)
    DescribeUserGroups (DescribeUserGroups'),
    newDescribeUserGroups,
    DescribeUserGroupsResponse (DescribeUserGroupsResponse'),
    newDescribeUserGroupsResponse,

    -- ** DescribeCacheClusters (Paginated)
    DescribeCacheClusters (DescribeCacheClusters'),
    newDescribeCacheClusters,
    DescribeCacheClustersResponse (DescribeCacheClustersResponse'),
    newDescribeCacheClustersResponse,

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

    -- ** DescribeReplicationGroups (Paginated)
    DescribeReplicationGroups (DescribeReplicationGroups'),
    newDescribeReplicationGroups,
    DescribeReplicationGroupsResponse (DescribeReplicationGroupsResponse'),
    newDescribeReplicationGroupsResponse,

    -- ** DescribeReservedCacheNodesOfferings (Paginated)
    DescribeReservedCacheNodesOfferings (DescribeReservedCacheNodesOfferings'),
    newDescribeReservedCacheNodesOfferings,
    DescribeReservedCacheNodesOfferingsResponse (DescribeReservedCacheNodesOfferingsResponse'),
    newDescribeReservedCacheNodesOfferingsResponse,

    -- ** DescribeEngineDefaultParameters (Paginated)
    DescribeEngineDefaultParameters (DescribeEngineDefaultParameters'),
    newDescribeEngineDefaultParameters,
    DescribeEngineDefaultParametersResponse (DescribeEngineDefaultParametersResponse'),
    newDescribeEngineDefaultParametersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    TagListMessage (TagListMessage'),
    newTagListMessage,

    -- ** RevokeCacheSecurityGroupIngress
    RevokeCacheSecurityGroupIngress (RevokeCacheSecurityGroupIngress'),
    newRevokeCacheSecurityGroupIngress,
    RevokeCacheSecurityGroupIngressResponse (RevokeCacheSecurityGroupIngressResponse'),
    newRevokeCacheSecurityGroupIngressResponse,

    -- ** CreateUserGroup
    CreateUserGroup (CreateUserGroup'),
    newCreateUserGroup,
    UserGroup (UserGroup'),
    newUserGroup,

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
