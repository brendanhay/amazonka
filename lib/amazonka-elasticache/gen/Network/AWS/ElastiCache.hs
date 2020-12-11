{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    elastiCacheService,

    -- * Errors
    -- $errors

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
    Authentication (..),
    mkAuthentication,
    aPasswordCount,
    aType,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- ** CacheCluster
    CacheCluster (..),
    mkCacheCluster,
    ccAuthTokenLastModifiedDate,
    ccEngineVersion,
    ccCacheNodeType,
    ccCacheNodes,
    ccCacheClusterCreateTime,
    ccAtRestEncryptionEnabled,
    ccAutoMinorVersionUpgrade,
    ccSecurityGroups,
    ccNotificationConfiguration,
    ccARN,
    ccTransitEncryptionEnabled,
    ccSnapshotWindow,
    ccCacheClusterId,
    ccConfigurationEndpoint,
    ccEngine,
    ccCacheSecurityGroups,
    ccAuthTokenEnabled,
    ccClientDownloadLandingPage,
    ccPreferredMaintenanceWindow,
    ccCacheSubnetGroupName,
    ccPreferredAvailabilityZone,
    ccCacheParameterGroup,
    ccCacheClusterStatus,
    ccSnapshotRetentionLimit,
    ccPreferredOutpostARN,
    ccReplicationGroupId,
    ccPendingModifiedValues,
    ccNumCacheNodes,

    -- ** CacheEngineVersion
    CacheEngineVersion (..),
    mkCacheEngineVersion,
    cevEngineVersion,
    cevCacheParameterGroupFamily,
    cevCacheEngineDescription,
    cevEngine,
    cevCacheEngineVersionDescription,

    -- ** CacheNode
    CacheNode (..),
    mkCacheNode,
    cnSourceCacheNodeId,
    cnParameterGroupStatus,
    cnCacheNodeCreateTime,
    cnCustomerAvailabilityZone,
    cnCacheNodeId,
    cnCustomerOutpostARN,
    cnCacheNodeStatus,
    cnEndpoint,

    -- ** CacheNodeTypeSpecificParameter
    CacheNodeTypeSpecificParameter (..),
    mkCacheNodeTypeSpecificParameter,
    cntspCacheNodeTypeSpecificValues,
    cntspMinimumEngineVersion,
    cntspSource,
    cntspIsModifiable,
    cntspDataType,
    cntspAllowedValues,
    cntspParameterName,
    cntspDescription,
    cntspChangeType,

    -- ** CacheNodeTypeSpecificValue
    CacheNodeTypeSpecificValue (..),
    mkCacheNodeTypeSpecificValue,
    cntsvCacheNodeType,
    cntsvValue,

    -- ** CacheNodeUpdateStatus
    CacheNodeUpdateStatus (..),
    mkCacheNodeUpdateStatus,
    cnusNodeUpdateEndDate,
    cnusNodeUpdateInitiatedBy,
    cnusNodeUpdateStatusModifiedDate,
    cnusCacheNodeId,
    cnusNodeUpdateInitiatedDate,
    cnusNodeUpdateStartDate,
    cnusNodeUpdateStatus,
    cnusNodeDeletionDate,

    -- ** CacheParameterGroup
    CacheParameterGroup (..),
    mkCacheParameterGroup,
    cpgCacheParameterGroupFamily,
    cpgARN,
    cpgCacheParameterGroupName,
    cpgIsGlobal,
    cpgDescription,

    -- ** CacheParameterGroupNameMessage
    CacheParameterGroupNameMessage (..),
    mkCacheParameterGroupNameMessage,
    cpgnmCacheParameterGroupName,

    -- ** CacheParameterGroupStatus
    CacheParameterGroupStatus (..),
    mkCacheParameterGroupStatus,
    cpgsCacheParameterGroupName,
    cpgsCacheNodeIdsToReboot,
    cpgsParameterApplyStatus,

    -- ** CacheSecurityGroup
    CacheSecurityGroup (..),
    mkCacheSecurityGroup,
    csgCacheSecurityGroupName,
    csgARN,
    csgOwnerId,
    csgEC2SecurityGroups,
    csgDescription,

    -- ** CacheSecurityGroupMembership
    CacheSecurityGroupMembership (..),
    mkCacheSecurityGroupMembership,
    csgmStatus,
    csgmCacheSecurityGroupName,

    -- ** CacheSubnetGroup
    CacheSubnetGroup (..),
    mkCacheSubnetGroup,
    cARN,
    cVPCId,
    cSubnets,
    cCacheSubnetGroupName,
    cCacheSubnetGroupDescription,

    -- ** ConfigureShard
    ConfigureShard (..),
    mkConfigureShard,
    csPreferredAvailabilityZones,
    csPreferredOutpostARNs,
    csNodeGroupId,
    csNewReplicaCount,

    -- ** CustomerNodeEndpoint
    CustomerNodeEndpoint (..),
    mkCustomerNodeEndpoint,
    cneAddress,
    cnePort,

    -- ** EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,

    -- ** Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- ** EngineDefaults
    EngineDefaults (..),
    mkEngineDefaults,
    edCacheParameterGroupFamily,
    edCacheNodeTypeSpecificParameters,
    edMarker,
    edParameters,

    -- ** Event
    Event (..),
    mkEvent,
    eSourceType,
    eSourceIdentifier,
    eDate,
    eMessage,

    -- ** Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- ** GlobalNodeGroup
    GlobalNodeGroup (..),
    mkGlobalNodeGroup,
    gngSlots,
    gngGlobalNodeGroupId,

    -- ** GlobalReplicationGroup
    GlobalReplicationGroup (..),
    mkGlobalReplicationGroup,
    grgEngineVersion,
    grgStatus,
    grgCacheNodeType,
    grgClusterEnabled,
    grgAtRestEncryptionEnabled,
    grgARN,
    grgTransitEncryptionEnabled,
    grgMembers,
    grgEngine,
    grgAuthTokenEnabled,
    grgGlobalNodeGroups,
    grgGlobalReplicationGroupId,
    grgGlobalReplicationGroupDescription,

    -- ** GlobalReplicationGroupInfo
    GlobalReplicationGroupInfo (..),
    mkGlobalReplicationGroupInfo,
    grgiGlobalReplicationGroupMemberRole,
    grgiGlobalReplicationGroupId,

    -- ** GlobalReplicationGroupMember
    GlobalReplicationGroupMember (..),
    mkGlobalReplicationGroupMember,
    grgmStatus,
    grgmReplicationGroupRegion,
    grgmRole,
    grgmReplicationGroupId,
    grgmAutomaticFailover,

    -- ** NodeGroup
    NodeGroup (..),
    mkNodeGroup,
    ngStatus,
    ngPrimaryEndpoint,
    ngSlots,
    ngNodeGroupMembers,
    ngNodeGroupId,
    ngReaderEndpoint,

    -- ** NodeGroupConfiguration
    NodeGroupConfiguration (..),
    mkNodeGroupConfiguration,
    ngcSlots,
    ngcReplicaOutpostARNs,
    ngcReplicaCount,
    ngcPrimaryAvailabilityZone,
    ngcReplicaAvailabilityZones,
    ngcPrimaryOutpostARN,
    ngcNodeGroupId,

    -- ** NodeGroupMember
    NodeGroupMember (..),
    mkNodeGroupMember,
    ngmCacheClusterId,
    ngmCacheNodeId,
    ngmPreferredAvailabilityZone,
    ngmCurrentRole,
    ngmPreferredOutpostARN,
    ngmReadEndpoint,

    -- ** NodeGroupMemberUpdateStatus
    NodeGroupMemberUpdateStatus (..),
    mkNodeGroupMemberUpdateStatus,
    ngmusNodeUpdateEndDate,
    ngmusNodeUpdateInitiatedBy,
    ngmusNodeUpdateStatusModifiedDate,
    ngmusCacheClusterId,
    ngmusCacheNodeId,
    ngmusNodeUpdateInitiatedDate,
    ngmusNodeUpdateStartDate,
    ngmusNodeUpdateStatus,
    ngmusNodeDeletionDate,

    -- ** NodeGroupUpdateStatus
    NodeGroupUpdateStatus (..),
    mkNodeGroupUpdateStatus,
    ngusNodeGroupMemberUpdateStatus,
    ngusNodeGroupId,

    -- ** NodeSnapshot
    NodeSnapshot (..),
    mkNodeSnapshot,
    nsNodeGroupConfiguration,
    nsCacheNodeCreateTime,
    nsCacheClusterId,
    nsCacheNodeId,
    nsNodeGroupId,
    nsSnapshotCreateTime,
    nsCacheSize,

    -- ** NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncTopicStatus,
    ncTopicARN,

    -- ** Parameter
    Parameter (..),
    mkParameter,
    pParameterValue,
    pMinimumEngineVersion,
    pSource,
    pIsModifiable,
    pDataType,
    pAllowedValues,
    pParameterName,
    pDescription,
    pChangeType,

    -- ** ParameterNameValue
    ParameterNameValue (..),
    mkParameterNameValue,
    pnvParameterValue,
    pnvParameterName,

    -- ** PendingModifiedValues
    PendingModifiedValues (..),
    mkPendingModifiedValues,
    pmvEngineVersion,
    pmvCacheNodeType,
    pmvAuthTokenStatus,
    pmvCacheNodeIdsToRemove,
    pmvNumCacheNodes,

    -- ** ProcessedUpdateAction
    ProcessedUpdateAction (..),
    mkProcessedUpdateAction,
    puaCacheClusterId,
    puaServiceUpdateName,
    puaUpdateActionStatus,
    puaReplicationGroupId,

    -- ** RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- ** RegionalConfiguration
    RegionalConfiguration (..),
    mkRegionalConfiguration,
    rcReplicationGroupId,
    rcReplicationGroupRegion,
    rcReshardingConfiguration,

    -- ** ReplicationGroup
    ReplicationGroup (..),
    mkReplicationGroup,
    rgAuthTokenLastModifiedDate,
    rgStatus,
    rgCacheNodeType,
    rgNodeGroups,
    rgSnapshottingClusterId,
    rgClusterEnabled,
    rgAtRestEncryptionEnabled,
    rgARN,
    rgTransitEncryptionEnabled,
    rgUserGroupIds,
    rgSnapshotWindow,
    rgConfigurationEndpoint,
    rgAuthTokenEnabled,
    rgMemberClusters,
    rgKMSKeyId,
    rgMultiAZ,
    rgSnapshotRetentionLimit,
    rgDescription,
    rgReplicationGroupId,
    rgPendingModifiedValues,
    rgGlobalReplicationGroupInfo,
    rgMemberClustersOutpostARNs,
    rgAutomaticFailover,

    -- ** ReplicationGroupPendingModifiedValues
    ReplicationGroupPendingModifiedValues (..),
    mkReplicationGroupPendingModifiedValues,
    rgpmvAuthTokenStatus,
    rgpmvUserGroups,
    rgpmvResharding,
    rgpmvPrimaryClusterId,
    rgpmvAutomaticFailoverStatus,

    -- ** ReservedCacheNode
    ReservedCacheNode (..),
    mkReservedCacheNode,
    rcnCacheNodeType,
    rcnState,
    rcnStartTime,
    rcnProductDescription,
    rcnReservationARN,
    rcnCacheNodeCount,
    rcnReservedCacheNodeId,
    rcnRecurringCharges,
    rcnOfferingType,
    rcnUsagePrice,
    rcnFixedPrice,
    rcnDuration,
    rcnReservedCacheNodesOfferingId,

    -- ** ReservedCacheNodesOffering
    ReservedCacheNodesOffering (..),
    mkReservedCacheNodesOffering,
    rcnoCacheNodeType,
    rcnoProductDescription,
    rcnoRecurringCharges,
    rcnoOfferingType,
    rcnoUsagePrice,
    rcnoFixedPrice,
    rcnoDuration,
    rcnoReservedCacheNodesOfferingId,

    -- ** ReshardingConfiguration
    ReshardingConfiguration (..),
    mkReshardingConfiguration,
    rcPreferredAvailabilityZones,
    rcNodeGroupId,

    -- ** ReshardingStatus
    ReshardingStatus (..),
    mkReshardingStatus,
    rsSlotMigration,

    -- ** SecurityGroupMembership
    SecurityGroupMembership (..),
    mkSecurityGroupMembership,
    sgmStatus,
    sgmSecurityGroupId,

    -- ** ServiceUpdate
    ServiceUpdate (..),
    mkServiceUpdate,
    suEngineVersion,
    suServiceUpdateType,
    suServiceUpdateName,
    suEngine,
    suServiceUpdateReleaseDate,
    suAutoUpdateAfterRecommendedApplyByDate,
    suServiceUpdateSeverity,
    suServiceUpdateEndDate,
    suServiceUpdateDescription,
    suServiceUpdateRecommendedApplyByDate,
    suServiceUpdateStatus,
    suEstimatedUpdateTime,

    -- ** SlotMigration
    SlotMigration (..),
    mkSlotMigration,
    smProgressPercentage,

    -- ** Snapshot
    Snapshot (..),
    mkSnapshot,
    sEngineVersion,
    sCacheNodeType,
    sCacheClusterCreateTime,
    sAutoMinorVersionUpgrade,
    sARN,
    sCacheParameterGroupName,
    sReplicationGroupDescription,
    sVPCId,
    sSnapshotStatus,
    sSnapshotWindow,
    sCacheClusterId,
    sEngine,
    sPreferredMaintenanceWindow,
    sTopicARN,
    sKMSKeyId,
    sNodeSnapshots,
    sCacheSubnetGroupName,
    sPreferredAvailabilityZone,
    sNumNodeGroups,
    sSnapshotRetentionLimit,
    sSnapshotName,
    sPreferredOutpostARN,
    sReplicationGroupId,
    sNumCacheNodes,
    sPort,
    sAutomaticFailover,
    sSnapshotSource,

    -- ** Subnet
    Subnet (..),
    mkSubnet,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
    sSubnetOutpost,

    -- ** SubnetOutpost
    SubnetOutpost (..),
    mkSubnetOutpost,
    soSubnetOutpostARN,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** TagListMessage
    TagListMessage (..),
    mkTagListMessage,
    tlmTagList,

    -- ** TimeRangeFilter
    TimeRangeFilter (..),
    mkTimeRangeFilter,
    trfStartTime,
    trfEndTime,

    -- ** UnprocessedUpdateAction
    UnprocessedUpdateAction (..),
    mkUnprocessedUpdateAction,
    uuaCacheClusterId,
    uuaServiceUpdateName,
    uuaErrorType,
    uuaErrorMessage,
    uuaReplicationGroupId,

    -- ** UpdateAction
    UpdateAction (..),
    mkUpdateAction,
    uaServiceUpdateType,
    uaSlaMet,
    uaCacheClusterId,
    uaServiceUpdateName,
    uaUpdateActionStatus,
    uaEngine,
    uaNodesUpdated,
    uaUpdateActionStatusModifiedDate,
    uaServiceUpdateReleaseDate,
    uaCacheNodeUpdateStatus,
    uaServiceUpdateSeverity,
    uaNodeGroupUpdateStatus,
    uaServiceUpdateRecommendedApplyByDate,
    uaUpdateActionAvailableDate,
    uaServiceUpdateStatus,
    uaEstimatedUpdateTime,
    uaReplicationGroupId,

    -- ** UpdateActionResultsMessage
    UpdateActionResultsMessage (..),
    mkUpdateActionResultsMessage,
    uarmUnprocessedUpdateActions,
    uarmProcessedUpdateActions,

    -- ** User
    User (..),
    mkUser,
    uStatus,
    uARN,
    uUserGroupIds,
    uAuthentication,
    uEngine,
    uUserName,
    uAccessString,
    uUserId,

    -- ** UserGroup
    UserGroup (..),
    mkUserGroup,
    ugStatus,
    ugUserIds,
    ugARN,
    ugUserGroupId,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,

    -- ** UserGroupPendingChanges
    UserGroupPendingChanges (..),
    mkUserGroupPendingChanges,
    ugpcUserIdsToAdd,
    ugpcUserIdsToRemove,

    -- ** UserGroupsUpdateStatus
    UserGroupsUpdateStatus (..),
    mkUserGroupsUpdateStatus,
    ugusUserGroupIdsToAdd,
    ugusUserGroupIdsToRemove,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
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
