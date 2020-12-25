{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __Amazon ElastiCache__
--
-- Amazon ElastiCache is a web service that makes it easier to set up, operate, and scale a distributed cache in the cloud.
-- With ElastiCache, customers get all of the benefits of a high-performance, in-memory cache with less of the administrative burden involved in launching and managing a distributed cache. The service makes setup, scaling, and cluster failure handling much simpler than in a self-managed cache deployment.
-- In addition, through integration with Amazon CloudWatch, customers get enhanced visibility into the key performance statistics associated with their cache and can receive alarms if a part of their cache runs hot.
module Network.AWS.ElastiCache
  ( -- * Service configuration
    mkServiceConfig,

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
    mkCacheClusterAvailable,

    -- ** CacheClusterDeleted
    mkCacheClusterDeleted,

    -- ** ReplicationGroupDeleted
    mkReplicationGroupDeleted,

    -- ** ReplicationGroupAvailable
    mkReplicationGroupAvailable,

    -- * Operations
    -- $operations

    -- ** StartMigration
    module Network.AWS.ElastiCache.StartMigration,

    -- ** DeleteCacheSecurityGroup
    module Network.AWS.ElastiCache.DeleteCacheSecurityGroup,

    -- ** CreateReplicationGroup
    module Network.AWS.ElastiCache.CreateReplicationGroup,

    -- ** DeleteCacheCluster
    module Network.AWS.ElastiCache.DeleteCacheCluster,

    -- ** IncreaseNodeGroupsInGlobalReplicationGroup
    module Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup,

    -- ** DescribeUsers (Paginated)
    module Network.AWS.ElastiCache.DescribeUsers,

    -- ** RebootCacheCluster
    module Network.AWS.ElastiCache.RebootCacheCluster,

    -- ** CreateUserGroup
    module Network.AWS.ElastiCache.CreateUserGroup,

    -- ** RevokeCacheSecurityGroupIngress
    module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress,

    -- ** CreateCacheCluster
    module Network.AWS.ElastiCache.CreateCacheCluster,

    -- ** DescribeEvents (Paginated)
    module Network.AWS.ElastiCache.DescribeEvents,

    -- ** DescribeEngineDefaultParameters (Paginated)
    module Network.AWS.ElastiCache.DescribeEngineDefaultParameters,

    -- ** DisassociateGlobalReplicationGroup
    module Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup,

    -- ** ModifyCacheParameterGroup
    module Network.AWS.ElastiCache.ModifyCacheParameterGroup,

    -- ** TestFailover
    module Network.AWS.ElastiCache.TestFailover,

    -- ** DeleteReplicationGroup
    module Network.AWS.ElastiCache.DeleteReplicationGroup,

    -- ** ListTagsForResource
    module Network.AWS.ElastiCache.ListTagsForResource,

    -- ** CompleteMigration
    module Network.AWS.ElastiCache.CompleteMigration,

    -- ** DescribeCacheClusters (Paginated)
    module Network.AWS.ElastiCache.DescribeCacheClusters,

    -- ** PurchaseReservedCacheNodesOffering
    module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering,

    -- ** RemoveTagsFromResource
    module Network.AWS.ElastiCache.RemoveTagsFromResource,

    -- ** ModifyReplicationGroup
    module Network.AWS.ElastiCache.ModifyReplicationGroup,

    -- ** DescribeCacheParameters (Paginated)
    module Network.AWS.ElastiCache.DescribeCacheParameters,

    -- ** DescribeGlobalReplicationGroups (Paginated)
    module Network.AWS.ElastiCache.DescribeGlobalReplicationGroups,

    -- ** DescribeCacheSubnetGroups (Paginated)
    module Network.AWS.ElastiCache.DescribeCacheSubnetGroups,

    -- ** DescribeUpdateActions (Paginated)
    module Network.AWS.ElastiCache.DescribeUpdateActions,

    -- ** RebalanceSlotsInGlobalReplicationGroup
    module Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup,

    -- ** CreateCacheSecurityGroup
    module Network.AWS.ElastiCache.CreateCacheSecurityGroup,

    -- ** DecreaseReplicaCount
    module Network.AWS.ElastiCache.DecreaseReplicaCount,

    -- ** AddTagsToResource
    module Network.AWS.ElastiCache.AddTagsToResource,

    -- ** AuthorizeCacheSecurityGroupIngress
    module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress,

    -- ** CopySnapshot
    module Network.AWS.ElastiCache.CopySnapshot,

    -- ** FailoverGlobalReplicationGroup
    module Network.AWS.ElastiCache.FailoverGlobalReplicationGroup,

    -- ** CreateCacheSubnetGroup
    module Network.AWS.ElastiCache.CreateCacheSubnetGroup,

    -- ** CreateGlobalReplicationGroup
    module Network.AWS.ElastiCache.CreateGlobalReplicationGroup,

    -- ** DescribeCacheParameterGroups (Paginated)
    module Network.AWS.ElastiCache.DescribeCacheParameterGroups,

    -- ** ResetCacheParameterGroup
    module Network.AWS.ElastiCache.ResetCacheParameterGroup,

    -- ** ListAllowedNodeTypeModifications
    module Network.AWS.ElastiCache.ListAllowedNodeTypeModifications,

    -- ** IncreaseReplicaCount
    module Network.AWS.ElastiCache.IncreaseReplicaCount,

    -- ** ModifyReplicationGroupShardConfiguration
    module Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration,

    -- ** BatchApplyUpdateAction
    module Network.AWS.ElastiCache.BatchApplyUpdateAction,

    -- ** DeleteUserGroup
    module Network.AWS.ElastiCache.DeleteUserGroup,

    -- ** DescribeServiceUpdates (Paginated)
    module Network.AWS.ElastiCache.DescribeServiceUpdates,

    -- ** DescribeSnapshots (Paginated)
    module Network.AWS.ElastiCache.DescribeSnapshots,

    -- ** DescribeReplicationGroups (Paginated)
    module Network.AWS.ElastiCache.DescribeReplicationGroups,

    -- ** CreateUser
    module Network.AWS.ElastiCache.CreateUser,

    -- ** DeleteSnapshot
    module Network.AWS.ElastiCache.DeleteSnapshot,

    -- ** DescribeReservedCacheNodesOfferings (Paginated)
    module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings,

    -- ** ModifyCacheSubnetGroup
    module Network.AWS.ElastiCache.ModifyCacheSubnetGroup,

    -- ** DeleteUser
    module Network.AWS.ElastiCache.DeleteUser,

    -- ** CreateSnapshot
    module Network.AWS.ElastiCache.CreateSnapshot,

    -- ** ModifyGlobalReplicationGroup
    module Network.AWS.ElastiCache.ModifyGlobalReplicationGroup,

    -- ** DescribeUserGroups (Paginated)
    module Network.AWS.ElastiCache.DescribeUserGroups,

    -- ** DeleteCacheParameterGroup
    module Network.AWS.ElastiCache.DeleteCacheParameterGroup,

    -- ** DescribeCacheSecurityGroups (Paginated)
    module Network.AWS.ElastiCache.DescribeCacheSecurityGroups,

    -- ** BatchStopUpdateAction
    module Network.AWS.ElastiCache.BatchStopUpdateAction,

    -- ** ModifyCacheCluster
    module Network.AWS.ElastiCache.ModifyCacheCluster,

    -- ** DescribeCacheEngineVersions (Paginated)
    module Network.AWS.ElastiCache.DescribeCacheEngineVersions,

    -- ** ModifyUserGroup
    module Network.AWS.ElastiCache.ModifyUserGroup,

    -- ** CreateCacheParameterGroup
    module Network.AWS.ElastiCache.CreateCacheParameterGroup,

    -- ** DescribeReservedCacheNodes (Paginated)
    module Network.AWS.ElastiCache.DescribeReservedCacheNodes,

    -- ** DeleteGlobalReplicationGroup
    module Network.AWS.ElastiCache.DeleteGlobalReplicationGroup,

    -- ** DecreaseNodeGroupsInGlobalReplicationGroup
    module Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup,

    -- ** ModifyUser
    module Network.AWS.ElastiCache.ModifyUser,

    -- ** DeleteCacheSubnetGroup
    module Network.AWS.ElastiCache.DeleteCacheSubnetGroup,

    -- * Types

    -- ** NodeSnapshot
    NodeSnapshot (..),
    mkNodeSnapshot,
    nsCacheClusterId,
    nsCacheNodeCreateTime,
    nsCacheNodeId,
    nsCacheSize,
    nsNodeGroupConfiguration,
    nsNodeGroupId,
    nsSnapshotCreateTime,

    -- ** ServiceUpdateType
    ServiceUpdateType (..),

    -- ** GlobalNodeGroup
    GlobalNodeGroup (..),
    mkGlobalNodeGroup,
    gngGlobalNodeGroupId,
    gngSlots,

    -- ** UserGroupPendingChanges
    UserGroupPendingChanges (..),
    mkUserGroupPendingChanges,
    ugpcUserIdsToAdd,
    ugpcUserIdsToRemove,

    -- ** Snapshot
    Snapshot (..),
    mkSnapshot,
    sARN,
    sAutoMinorVersionUpgrade,
    sAutomaticFailover,
    sCacheClusterCreateTime,
    sCacheClusterId,
    sCacheNodeType,
    sCacheParameterGroupName,
    sCacheSubnetGroupName,
    sEngine,
    sEngineVersion,
    sKmsKeyId,
    sNodeSnapshots,
    sNumCacheNodes,
    sNumNodeGroups,
    sPort,
    sPreferredAvailabilityZone,
    sPreferredMaintenanceWindow,
    sPreferredOutpostArn,
    sReplicationGroupDescription,
    sReplicationGroupId,
    sSnapshotName,
    sSnapshotRetentionLimit,
    sSnapshotSource,
    sSnapshotStatus,
    sSnapshotWindow,
    sTopicArn,
    sVpcId,

    -- ** Event
    Event (..),
    mkEvent,
    eDate,
    eMessage,
    eSourceIdentifier,
    eSourceType,

    -- ** UpdateAction
    UpdateAction (..),
    mkUpdateAction,
    uaCacheClusterId,
    uaCacheNodeUpdateStatus,
    uaEngine,
    uaEstimatedUpdateTime,
    uaNodeGroupUpdateStatus,
    uaNodesUpdated,
    uaReplicationGroupId,
    uaServiceUpdateName,
    uaServiceUpdateRecommendedApplyByDate,
    uaServiceUpdateReleaseDate,
    uaServiceUpdateSeverity,
    uaServiceUpdateStatus,
    uaServiceUpdateType,
    uaSlaMet,
    uaUpdateActionAvailableDate,
    uaUpdateActionStatus,
    uaUpdateActionStatusModifiedDate,

    -- ** NodeGroupConfiguration
    NodeGroupConfiguration (..),
    mkNodeGroupConfiguration,
    ngcNodeGroupId,
    ngcPrimaryAvailabilityZone,
    ngcPrimaryOutpostArn,
    ngcReplicaAvailabilityZones,
    ngcReplicaCount,
    ngcReplicaOutpostArns,
    ngcSlots,

    -- ** FilterName
    FilterName (..),

    -- ** ReshardingStatus
    ReshardingStatus (..),
    mkReshardingStatus,
    rsSlotMigration,

    -- ** NodeGroup
    NodeGroup (..),
    mkNodeGroup,
    ngNodeGroupId,
    ngNodeGroupMembers,
    ngPrimaryEndpoint,
    ngReaderEndpoint,
    ngSlots,
    ngStatus,

    -- ** CacheNodeTypeSpecificValue
    CacheNodeTypeSpecificValue (..),
    mkCacheNodeTypeSpecificValue,
    cntsvCacheNodeType,
    cntsvValue,

    -- ** CustomerNodeEndpoint
    CustomerNodeEndpoint (..),
    mkCustomerNodeEndpoint,
    cneAddress,
    cnePort,

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** NodeGroupMemberUpdateStatus
    NodeGroupMemberUpdateStatus (..),
    mkNodeGroupMemberUpdateStatus,
    ngmusCacheClusterId,
    ngmusCacheNodeId,
    ngmusNodeDeletionDate,
    ngmusNodeUpdateEndDate,
    ngmusNodeUpdateInitiatedBy,
    ngmusNodeUpdateInitiatedDate,
    ngmusNodeUpdateStartDate,
    ngmusNodeUpdateStatus,
    ngmusNodeUpdateStatusModifiedDate,

    -- ** AllowedNodeGroupId
    AllowedNodeGroupId (..),

    -- ** PendingAutomaticFailoverStatus
    PendingAutomaticFailoverStatus (..),

    -- ** SlaMet
    SlaMet (..),

    -- ** NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncTopicArn,
    ncTopicStatus,

    -- ** OutpostMode
    OutpostMode (..),

    -- ** ReplicationGroupPendingModifiedValues
    ReplicationGroupPendingModifiedValues (..),
    mkReplicationGroupPendingModifiedValues,
    rgpmvAuthTokenStatus,
    rgpmvAutomaticFailoverStatus,
    rgpmvPrimaryClusterId,
    rgpmvResharding,
    rgpmvUserGroups,

    -- ** EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    ecsgEC2SecurityGroupName,
    ecsgEC2SecurityGroupOwnerId,
    ecsgStatus,

    -- ** ParameterNameValue
    ParameterNameValue (..),
    mkParameterNameValue,
    pnvParameterName,
    pnvParameterValue,

    -- ** String
    String (..),

    -- ** SourceType
    SourceType (..),

    -- ** NodeUpdateInitiatedBy
    NodeUpdateInitiatedBy (..),

    -- ** RegionalConfiguration
    RegionalConfiguration (..),
    mkRegionalConfiguration,
    rcReplicationGroupId,
    rcReplicationGroupRegion,
    rcReshardingConfiguration,

    -- ** GlobalReplicationGroup
    GlobalReplicationGroup (..),
    mkGlobalReplicationGroup,
    grgARN,
    grgAtRestEncryptionEnabled,
    grgAuthTokenEnabled,
    grgCacheNodeType,
    grgClusterEnabled,
    grgEngine,
    grgEngineVersion,
    grgGlobalNodeGroups,
    grgGlobalReplicationGroupDescription,
    grgGlobalReplicationGroupId,
    grgMembers,
    grgStatus,
    grgTransitEncryptionEnabled,

    -- ** CacheSubnetGroup
    CacheSubnetGroup (..),
    mkCacheSubnetGroup,
    csgARN,
    csgCacheSubnetGroupDescription,
    csgCacheSubnetGroupName,
    csgSubnets,
    csgVpcId,

    -- ** Authentication
    Authentication (..),
    mkAuthentication,
    aPasswordCount,
    aType,

    -- ** ReservedCacheNode
    ReservedCacheNode (..),
    mkReservedCacheNode,
    rcnCacheNodeCount,
    rcnCacheNodeType,
    rcnDuration,
    rcnFixedPrice,
    rcnOfferingType,
    rcnProductDescription,
    rcnRecurringCharges,
    rcnReservationARN,
    rcnReservedCacheNodeId,
    rcnReservedCacheNodesOfferingId,
    rcnStartTime,
    rcnState,
    rcnUsagePrice,

    -- ** Subnet
    Subnet (..),
    mkSubnet,
    sSubnetAvailabilityZone,
    sSubnetIdentifier,
    sSubnetOutpost,

    -- ** ReshardingConfiguration
    ReshardingConfiguration (..),
    mkReshardingConfiguration,
    rcNodeGroupId,
    rcPreferredAvailabilityZones,

    -- ** SecurityGroupMembership
    SecurityGroupMembership (..),
    mkSecurityGroupMembership,
    sgmSecurityGroupId,
    sgmStatus,

    -- ** UserGroupId
    UserGroupId (..),

    -- ** UpdateActionStatus
    UpdateActionStatus (..),

    -- ** UserGroup
    UserGroup (..),
    mkUserGroup,
    ugARN,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,
    ugStatus,
    ugUserGroupId,
    ugUserIds,

    -- ** CacheCluster
    CacheCluster (..),
    mkCacheCluster,
    ccARN,
    ccAtRestEncryptionEnabled,
    ccAuthTokenEnabled,
    ccAuthTokenLastModifiedDate,
    ccAutoMinorVersionUpgrade,
    ccCacheClusterCreateTime,
    ccCacheClusterId,
    ccCacheClusterStatus,
    ccCacheNodeType,
    ccCacheNodes,
    ccCacheParameterGroup,
    ccCacheSecurityGroups,
    ccCacheSubnetGroupName,
    ccClientDownloadLandingPage,
    ccConfigurationEndpoint,
    ccEngine,
    ccEngineVersion,
    ccNotificationConfiguration,
    ccNumCacheNodes,
    ccPendingModifiedValues,
    ccPreferredAvailabilityZone,
    ccPreferredMaintenanceWindow,
    ccPreferredOutpostArn,
    ccReplicationGroupId,
    ccSecurityGroups,
    ccSnapshotRetentionLimit,
    ccSnapshotWindow,
    ccTransitEncryptionEnabled,

    -- ** MultiAZStatus
    MultiAZStatus (..),

    -- ** UpdateActionResultsMessage
    UpdateActionResultsMessage (..),
    mkUpdateActionResultsMessage,
    uarmProcessedUpdateActions,
    uarmUnprocessedUpdateActions,

    -- ** UserName
    UserName (..),

    -- ** EngineDefaults
    EngineDefaults (..),
    mkEngineDefaults,
    edCacheNodeTypeSpecificParameters,
    edCacheParameterGroupFamily,
    edMarker,
    edParameters,

    -- ** CacheParameterGroupStatus
    CacheParameterGroupStatus (..),
    mkCacheParameterGroupStatus,
    cpgsCacheNodeIdsToReboot,
    cpgsCacheParameterGroupName,
    cpgsParameterApplyStatus,

    -- ** AuthTokenUpdateStatus
    AuthTokenUpdateStatus (..),

    -- ** UnprocessedUpdateAction
    UnprocessedUpdateAction (..),
    mkUnprocessedUpdateAction,
    uuaCacheClusterId,
    uuaErrorMessage,
    uuaErrorType,
    uuaReplicationGroupId,
    uuaServiceUpdateName,

    -- ** AuthTokenUpdateStrategyType
    AuthTokenUpdateStrategyType (..),

    -- ** AccessString
    AccessString (..),

    -- ** User
    User (..),
    mkUser,
    uARN,
    uAccessString,
    uAuthentication,
    uEngine,
    uStatus,
    uUserGroupIds,
    uUserId,
    uUserName,

    -- ** CacheNodeUpdateStatus
    CacheNodeUpdateStatus (..),
    mkCacheNodeUpdateStatus,
    cnusCacheNodeId,
    cnusNodeDeletionDate,
    cnusNodeUpdateEndDate,
    cnusNodeUpdateInitiatedBy,
    cnusNodeUpdateInitiatedDate,
    cnusNodeUpdateStartDate,
    cnusNodeUpdateStatus,
    cnusNodeUpdateStatusModifiedDate,

    -- ** UserId
    UserId (..),

    -- ** ServiceUpdate
    ServiceUpdate (..),
    mkServiceUpdate,
    suAutoUpdateAfterRecommendedApplyByDate,
    suEngine,
    suEngineVersion,
    suEstimatedUpdateTime,
    suServiceUpdateDescription,
    suServiceUpdateEndDate,
    suServiceUpdateName,
    suServiceUpdateRecommendedApplyByDate,
    suServiceUpdateReleaseDate,
    suServiceUpdateSeverity,
    suServiceUpdateStatus,
    suServiceUpdateType,

    -- ** CacheNode
    CacheNode (..),
    mkCacheNode,
    cnCacheNodeCreateTime,
    cnCacheNodeId,
    cnCacheNodeStatus,
    cnCustomerAvailabilityZone,
    cnCustomerOutpostArn,
    cnEndpoint,
    cnParameterGroupStatus,
    cnSourceCacheNodeId,

    -- ** ConfigureShard
    ConfigureShard (..),
    mkConfigureShard,
    csNodeGroupId,
    csNewReplicaCount,
    csPreferredAvailabilityZones,
    csPreferredOutpostArns,

    -- ** CacheSecurityGroupMembership
    CacheSecurityGroupMembership (..),
    mkCacheSecurityGroupMembership,
    csgmCacheSecurityGroupName,
    csgmStatus,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- ** ServiceUpdateSeverity
    ServiceUpdateSeverity (..),

    -- ** NodeGroupMember
    NodeGroupMember (..),
    mkNodeGroupMember,
    ngmCacheClusterId,
    ngmCacheNodeId,
    ngmCurrentRole,
    ngmPreferredAvailabilityZone,
    ngmPreferredOutpostArn,
    ngmReadEndpoint,

    -- ** TimeRangeFilter
    TimeRangeFilter (..),
    mkTimeRangeFilter,
    trfEndTime,
    trfStartTime,

    -- ** NodeGroupUpdateStatus
    NodeGroupUpdateStatus (..),
    mkNodeGroupUpdateStatus,
    ngusNodeGroupId,
    ngusNodeGroupMemberUpdateStatus,

    -- ** CacheParameterGroup
    CacheParameterGroup (..),
    mkCacheParameterGroup,
    cpgARN,
    cpgCacheParameterGroupFamily,
    cpgCacheParameterGroupName,
    cpgDescription,
    cpgIsGlobal,

    -- ** ProcessedUpdateAction
    ProcessedUpdateAction (..),
    mkProcessedUpdateAction,
    puaCacheClusterId,
    puaReplicationGroupId,
    puaServiceUpdateName,
    puaUpdateActionStatus,

    -- ** AutomaticFailoverStatus
    AutomaticFailoverStatus (..),

    -- ** SubnetOutpost
    SubnetOutpost (..),
    mkSubnetOutpost,
    soSubnetOutpostArn,

    -- ** NodeUpdateStatus
    NodeUpdateStatus (..),

    -- ** CacheSecurityGroup
    CacheSecurityGroup (..),
    mkCacheSecurityGroup,
    cARN,
    cCacheSecurityGroupName,
    cDescription,
    cEC2SecurityGroups,
    cOwnerId,

    -- ** CacheNodeTypeSpecificParameter
    CacheNodeTypeSpecificParameter (..),
    mkCacheNodeTypeSpecificParameter,
    cntspAllowedValues,
    cntspCacheNodeTypeSpecificValues,
    cntspChangeType,
    cntspDataType,
    cntspDescription,
    cntspIsModifiable,
    cntspMinimumEngineVersion,
    cntspParameterName,
    cntspSource,

    -- ** GlobalReplicationGroupMember
    GlobalReplicationGroupMember (..),
    mkGlobalReplicationGroupMember,
    grgmAutomaticFailover,
    grgmReplicationGroupId,
    grgmReplicationGroupRegion,
    grgmRole,
    grgmStatus,

    -- ** AuthenticationType
    AuthenticationType (..),

    -- ** AZMode
    AZMode (..),

    -- ** SlotMigration
    SlotMigration (..),
    mkSlotMigration,
    smProgressPercentage,

    -- ** UserGroupsUpdateStatus
    UserGroupsUpdateStatus (..),
    mkUserGroupsUpdateStatus,
    ugusUserGroupIdsToAdd,
    ugusUserGroupIdsToRemove,

    -- ** Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- ** CacheEngineVersion
    CacheEngineVersion (..),
    mkCacheEngineVersion,
    cevCacheEngineDescription,
    cevCacheEngineVersionDescription,
    cevCacheParameterGroupFamily,
    cevEngine,
    cevEngineVersion,

    -- ** ServiceUpdateStatus
    ServiceUpdateStatus (..),

    -- ** ReplicationGroup
    ReplicationGroup (..),
    mkReplicationGroup,
    rgARN,
    rgAtRestEncryptionEnabled,
    rgAuthTokenEnabled,
    rgAuthTokenLastModifiedDate,
    rgAutomaticFailover,
    rgCacheNodeType,
    rgClusterEnabled,
    rgConfigurationEndpoint,
    rgDescription,
    rgGlobalReplicationGroupInfo,
    rgKmsKeyId,
    rgMemberClusters,
    rgMemberClustersOutpostArns,
    rgMultiAZ,
    rgNodeGroups,
    rgPendingModifiedValues,
    rgReplicationGroupId,
    rgSnapshotRetentionLimit,
    rgSnapshotWindow,
    rgSnapshottingClusterId,
    rgStatus,
    rgTransitEncryptionEnabled,
    rgUserGroupIds,

    -- ** FilterValue
    FilterValue (..),

    -- ** RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeAmount,
    rcRecurringChargeFrequency,

    -- ** ReservedCacheNodesOffering
    ReservedCacheNodesOffering (..),
    mkReservedCacheNodesOffering,
    rcnoCacheNodeType,
    rcnoDuration,
    rcnoFixedPrice,
    rcnoOfferingType,
    rcnoProductDescription,
    rcnoRecurringCharges,
    rcnoReservedCacheNodesOfferingId,
    rcnoUsagePrice,

    -- ** TagListMessage
    TagListMessage (..),
    mkTagListMessage,
    tlmTagList,

    -- ** Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- ** ChangeType
    ChangeType (..),

    -- ** EngineType
    EngineType (..),

    -- ** PendingModifiedValues
    PendingModifiedValues (..),
    mkPendingModifiedValues,
    pmvAuthTokenStatus,
    pmvCacheNodeIdsToRemove,
    pmvCacheNodeType,
    pmvEngineVersion,
    pmvNumCacheNodes,

    -- ** CacheParameterGroupNameMessage
    CacheParameterGroupNameMessage (..),
    mkCacheParameterGroupNameMessage,
    cpgnmCacheParameterGroupName,

    -- ** GlobalReplicationGroupInfo
    GlobalReplicationGroupInfo (..),
    mkGlobalReplicationGroupInfo,
    grgiGlobalReplicationGroupId,
    grgiGlobalReplicationGroupMemberRole,

    -- ** Parameter
    Parameter (..),
    mkParameter,
    pAllowedValues,
    pChangeType,
    pDataType,
    pDescription,
    pIsModifiable,
    pMinimumEngineVersion,
    pParameterName,
    pParameterValue,
    pSource,

    -- ** CacheClusterId
    CacheClusterId (..),

    -- ** CacheNodeId
    CacheNodeId (..),

    -- ** CacheSize
    CacheSize (..),

    -- ** NodeGroupId
    NodeGroupId (..),

    -- ** Engine
    Engine (..),

    -- ** Marker
    Marker (..),

    -- ** ReplicationGroupId
    ReplicationGroupId (..),

    -- ** ReplicationGroupDescription
    ReplicationGroupDescription (..),

    -- ** AuthToken
    AuthToken (..),

    -- ** CacheNodeType
    CacheNodeType (..),

    -- ** CacheParameterGroupName
    CacheParameterGroupName (..),

    -- ** CacheSubnetGroupName
    CacheSubnetGroupName (..),

    -- ** EngineVersion
    EngineVersion (..),

    -- ** GlobalReplicationGroupId
    GlobalReplicationGroupId (..),

    -- ** KmsKeyId
    KmsKeyId (..),

    -- ** NotificationTopicArn
    NotificationTopicArn (..),

    -- ** PreferredMaintenanceWindow
    PreferredMaintenanceWindow (..),

    -- ** PrimaryClusterId
    PrimaryClusterId (..),

    -- ** SnapshotName
    SnapshotName (..),

    -- ** SnapshotWindow
    SnapshotWindow (..),

    -- ** GlobalNodeGroupId
    GlobalNodeGroupId (..),

    -- ** Slots
    Slots (..),

    -- ** FinalSnapshotIdentifier
    FinalSnapshotIdentifier (..),

    -- ** ARN
    ARN (..),

    -- ** PreferredAvailabilityZone
    PreferredAvailabilityZone (..),

    -- ** PreferredOutpostArn
    PreferredOutpostArn (..),

    -- ** SnapshotSource
    SnapshotSource (..),

    -- ** SnapshotStatus
    SnapshotStatus (..),

    -- ** TopicArn
    TopicArn (..),

    -- ** VpcId
    VpcId (..),

    -- ** Message
    Message (..),

    -- ** SourceIdentifier
    SourceIdentifier (..),

    -- ** EstimatedUpdateTime
    EstimatedUpdateTime (..),

    -- ** NodesUpdated
    NodesUpdated (..),

    -- ** ServiceUpdateName
    ServiceUpdateName (..),

    -- ** PrimaryAvailabilityZone
    PrimaryAvailabilityZone (..),

    -- ** PrimaryOutpostArn
    PrimaryOutpostArn (..),

    -- ** PrimaryRegion
    PrimaryRegion (..),

    -- ** PrimaryReplicationGroupId
    PrimaryReplicationGroupId (..),

    -- ** GlobalReplicationGroupIdSuffix
    GlobalReplicationGroupIdSuffix (..),

    -- ** GlobalReplicationGroupDescription
    GlobalReplicationGroupDescription (..),

    -- ** Status
    Status (..),

    -- ** Value
    Value (..),

    -- ** CacheSubnetGroupDescription
    CacheSubnetGroupDescription (..),

    -- ** Address
    Address (..),

    -- ** Key
    Key (..),

    -- ** SourceSnapshotName
    SourceSnapshotName (..),

    -- ** TargetSnapshotName
    TargetSnapshotName (..),

    -- ** TargetBucket
    TargetBucket (..),

    -- ** CacheSecurityGroupName
    CacheSecurityGroupName (..),

    -- ** EC2SecurityGroupName
    EC2SecurityGroupName (..),

    -- ** EC2SecurityGroupOwnerId
    EC2SecurityGroupOwnerId (..),

    -- ** Description
    Description (..),

    -- ** ResourceName
    ResourceName (..),

    -- ** TopicStatus
    TopicStatus (..),

    -- ** ParameterName
    ParameterName (..),

    -- ** ParameterValue
    ParameterValue (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
