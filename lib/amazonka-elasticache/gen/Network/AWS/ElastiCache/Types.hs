-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types
  ( -- * Service configuration
    elastiCacheService,

    -- * Errors

    -- * AZMode
    AZMode (..),

    -- * AuthTokenUpdateStatus
    AuthTokenUpdateStatus (..),

    -- * AuthTokenUpdateStrategyType
    AuthTokenUpdateStrategyType (..),

    -- * AuthenticationType
    AuthenticationType (..),

    -- * AutomaticFailoverStatus
    AutomaticFailoverStatus (..),

    -- * ChangeType
    ChangeType (..),

    -- * MultiAZStatus
    MultiAZStatus (..),

    -- * NodeUpdateInitiatedBy
    NodeUpdateInitiatedBy (..),

    -- * NodeUpdateStatus
    NodeUpdateStatus (..),

    -- * OutpostMode
    OutpostMode (..),

    -- * PendingAutomaticFailoverStatus
    PendingAutomaticFailoverStatus (..),

    -- * ServiceUpdateSeverity
    ServiceUpdateSeverity (..),

    -- * ServiceUpdateStatus
    ServiceUpdateStatus (..),

    -- * ServiceUpdateType
    ServiceUpdateType (..),

    -- * SlaMet
    SlaMet (..),

    -- * SourceType
    SourceType (..),

    -- * UpdateActionStatus
    UpdateActionStatus (..),

    -- * Authentication
    Authentication (..),
    mkAuthentication,
    aPasswordCount,
    aType,

    -- * AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azName,

    -- * CacheCluster
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

    -- * CacheEngineVersion
    CacheEngineVersion (..),
    mkCacheEngineVersion,
    cevEngineVersion,
    cevCacheParameterGroupFamily,
    cevCacheEngineDescription,
    cevEngine,
    cevCacheEngineVersionDescription,

    -- * CacheNode
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

    -- * CacheNodeTypeSpecificParameter
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

    -- * CacheNodeTypeSpecificValue
    CacheNodeTypeSpecificValue (..),
    mkCacheNodeTypeSpecificValue,
    cntsvCacheNodeType,
    cntsvValue,

    -- * CacheNodeUpdateStatus
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

    -- * CacheParameterGroup
    CacheParameterGroup (..),
    mkCacheParameterGroup,
    cpgCacheParameterGroupFamily,
    cpgARN,
    cpgCacheParameterGroupName,
    cpgIsGlobal,
    cpgDescription,

    -- * CacheParameterGroupNameMessage
    CacheParameterGroupNameMessage (..),
    mkCacheParameterGroupNameMessage,
    cpgnmCacheParameterGroupName,

    -- * CacheParameterGroupStatus
    CacheParameterGroupStatus (..),
    mkCacheParameterGroupStatus,
    cpgsCacheParameterGroupName,
    cpgsCacheNodeIdsToReboot,
    cpgsParameterApplyStatus,

    -- * CacheSecurityGroup
    CacheSecurityGroup (..),
    mkCacheSecurityGroup,
    csgCacheSecurityGroupName,
    csgARN,
    csgOwnerId,
    csgEC2SecurityGroups,
    csgDescription,

    -- * CacheSecurityGroupMembership
    CacheSecurityGroupMembership (..),
    mkCacheSecurityGroupMembership,
    csgmStatus,
    csgmCacheSecurityGroupName,

    -- * CacheSubnetGroup
    CacheSubnetGroup (..),
    mkCacheSubnetGroup,
    cARN,
    cVPCId,
    cSubnets,
    cCacheSubnetGroupName,
    cCacheSubnetGroupDescription,

    -- * ConfigureShard
    ConfigureShard (..),
    mkConfigureShard,
    csPreferredAvailabilityZones,
    csPreferredOutpostARNs,
    csNodeGroupId,
    csNewReplicaCount,

    -- * CustomerNodeEndpoint
    CustomerNodeEndpoint (..),
    mkCustomerNodeEndpoint,
    cneAddress,
    cnePort,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    mkEC2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,

    -- * Endpoint
    Endpoint (..),
    mkEndpoint,
    eAddress,
    ePort,

    -- * EngineDefaults
    EngineDefaults (..),
    mkEngineDefaults,
    edCacheParameterGroupFamily,
    edCacheNodeTypeSpecificParameters,
    edMarker,
    edParameters,

    -- * Event
    Event (..),
    mkEvent,
    eSourceType,
    eSourceIdentifier,
    eDate,
    eMessage,

    -- * Filter
    Filter (..),
    mkFilter,
    fName,
    fValues,

    -- * GlobalNodeGroup
    GlobalNodeGroup (..),
    mkGlobalNodeGroup,
    gngSlots,
    gngGlobalNodeGroupId,

    -- * GlobalReplicationGroup
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

    -- * GlobalReplicationGroupInfo
    GlobalReplicationGroupInfo (..),
    mkGlobalReplicationGroupInfo,
    grgiGlobalReplicationGroupMemberRole,
    grgiGlobalReplicationGroupId,

    -- * GlobalReplicationGroupMember
    GlobalReplicationGroupMember (..),
    mkGlobalReplicationGroupMember,
    grgmStatus,
    grgmReplicationGroupRegion,
    grgmRole,
    grgmReplicationGroupId,
    grgmAutomaticFailover,

    -- * NodeGroup
    NodeGroup (..),
    mkNodeGroup,
    ngStatus,
    ngPrimaryEndpoint,
    ngSlots,
    ngNodeGroupMembers,
    ngNodeGroupId,
    ngReaderEndpoint,

    -- * NodeGroupConfiguration
    NodeGroupConfiguration (..),
    mkNodeGroupConfiguration,
    ngcSlots,
    ngcReplicaOutpostARNs,
    ngcReplicaCount,
    ngcPrimaryAvailabilityZone,
    ngcReplicaAvailabilityZones,
    ngcPrimaryOutpostARN,
    ngcNodeGroupId,

    -- * NodeGroupMember
    NodeGroupMember (..),
    mkNodeGroupMember,
    ngmCacheClusterId,
    ngmCacheNodeId,
    ngmPreferredAvailabilityZone,
    ngmCurrentRole,
    ngmPreferredOutpostARN,
    ngmReadEndpoint,

    -- * NodeGroupMemberUpdateStatus
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

    -- * NodeGroupUpdateStatus
    NodeGroupUpdateStatus (..),
    mkNodeGroupUpdateStatus,
    ngusNodeGroupMemberUpdateStatus,
    ngusNodeGroupId,

    -- * NodeSnapshot
    NodeSnapshot (..),
    mkNodeSnapshot,
    nsNodeGroupConfiguration,
    nsCacheNodeCreateTime,
    nsCacheClusterId,
    nsCacheNodeId,
    nsNodeGroupId,
    nsSnapshotCreateTime,
    nsCacheSize,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    mkNotificationConfiguration,
    ncTopicStatus,
    ncTopicARN,

    -- * Parameter
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

    -- * ParameterNameValue
    ParameterNameValue (..),
    mkParameterNameValue,
    pnvParameterValue,
    pnvParameterName,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    mkPendingModifiedValues,
    pmvEngineVersion,
    pmvCacheNodeType,
    pmvAuthTokenStatus,
    pmvCacheNodeIdsToRemove,
    pmvNumCacheNodes,

    -- * ProcessedUpdateAction
    ProcessedUpdateAction (..),
    mkProcessedUpdateAction,
    puaCacheClusterId,
    puaServiceUpdateName,
    puaUpdateActionStatus,
    puaReplicationGroupId,

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * RegionalConfiguration
    RegionalConfiguration (..),
    mkRegionalConfiguration,
    rcReplicationGroupId,
    rcReplicationGroupRegion,
    rcReshardingConfiguration,

    -- * ReplicationGroup
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

    -- * ReplicationGroupPendingModifiedValues
    ReplicationGroupPendingModifiedValues (..),
    mkReplicationGroupPendingModifiedValues,
    rgpmvAuthTokenStatus,
    rgpmvUserGroups,
    rgpmvResharding,
    rgpmvPrimaryClusterId,
    rgpmvAutomaticFailoverStatus,

    -- * ReservedCacheNode
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

    -- * ReservedCacheNodesOffering
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

    -- * ReshardingConfiguration
    ReshardingConfiguration (..),
    mkReshardingConfiguration,
    rcPreferredAvailabilityZones,
    rcNodeGroupId,

    -- * ReshardingStatus
    ReshardingStatus (..),
    mkReshardingStatus,
    rsSlotMigration,

    -- * SecurityGroupMembership
    SecurityGroupMembership (..),
    mkSecurityGroupMembership,
    sgmStatus,
    sgmSecurityGroupId,

    -- * ServiceUpdate
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

    -- * SlotMigration
    SlotMigration (..),
    mkSlotMigration,
    smProgressPercentage,

    -- * Snapshot
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

    -- * Subnet
    Subnet (..),
    mkSubnet,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
    sSubnetOutpost,

    -- * SubnetOutpost
    SubnetOutpost (..),
    mkSubnetOutpost,
    soSubnetOutpostARN,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * TagListMessage
    TagListMessage (..),
    mkTagListMessage,
    tlmTagList,

    -- * TimeRangeFilter
    TimeRangeFilter (..),
    mkTimeRangeFilter,
    trfStartTime,
    trfEndTime,

    -- * UnprocessedUpdateAction
    UnprocessedUpdateAction (..),
    mkUnprocessedUpdateAction,
    uuaCacheClusterId,
    uuaServiceUpdateName,
    uuaErrorType,
    uuaErrorMessage,
    uuaReplicationGroupId,

    -- * UpdateAction
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

    -- * UpdateActionResultsMessage
    UpdateActionResultsMessage (..),
    mkUpdateActionResultsMessage,
    uarmUnprocessedUpdateActions,
    uarmProcessedUpdateActions,

    -- * User
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

    -- * UserGroup
    UserGroup (..),
    mkUserGroup,
    ugStatus,
    ugUserIds,
    ugARN,
    ugUserGroupId,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,

    -- * UserGroupPendingChanges
    UserGroupPendingChanges (..),
    mkUserGroupPendingChanges,
    ugpcUserIdsToAdd,
    ugpcUserIdsToRemove,

    -- * UserGroupsUpdateStatus
    UserGroupsUpdateStatus (..),
    mkUserGroupsUpdateStatus,
    ugusUserGroupIdsToAdd,
    ugusUserGroupIdsToRemove,
  )
where

import Network.AWS.ElastiCache.Types.AZMode
import Network.AWS.ElastiCache.Types.AuthTokenUpdateStatus
import Network.AWS.ElastiCache.Types.AuthTokenUpdateStrategyType
import Network.AWS.ElastiCache.Types.Authentication
import Network.AWS.ElastiCache.Types.AuthenticationType
import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.AvailabilityZone
import Network.AWS.ElastiCache.Types.CacheCluster
import Network.AWS.ElastiCache.Types.CacheEngineVersion
import Network.AWS.ElastiCache.Types.CacheNode
import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificParameter
import Network.AWS.ElastiCache.Types.CacheNodeTypeSpecificValue
import Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
import Network.AWS.ElastiCache.Types.CacheParameterGroup
import Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
import Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
import Network.AWS.ElastiCache.Types.CacheSecurityGroup
import Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
import Network.AWS.ElastiCache.Types.CacheSubnetGroup
import Network.AWS.ElastiCache.Types.ChangeType
import Network.AWS.ElastiCache.Types.ConfigureShard
import Network.AWS.ElastiCache.Types.CustomerNodeEndpoint
import Network.AWS.ElastiCache.Types.EC2SecurityGroup
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.EngineDefaults
import Network.AWS.ElastiCache.Types.Event
import Network.AWS.ElastiCache.Types.Filter
import Network.AWS.ElastiCache.Types.GlobalNodeGroup
import Network.AWS.ElastiCache.Types.GlobalReplicationGroup
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupInfo
import Network.AWS.ElastiCache.Types.GlobalReplicationGroupMember
import Network.AWS.ElastiCache.Types.MultiAZStatus
import Network.AWS.ElastiCache.Types.NodeGroup
import Network.AWS.ElastiCache.Types.NodeGroupConfiguration
import Network.AWS.ElastiCache.Types.NodeGroupMember
import Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
import Network.AWS.ElastiCache.Types.NodeGroupUpdateStatus
import Network.AWS.ElastiCache.Types.NodeSnapshot
import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import Network.AWS.ElastiCache.Types.NotificationConfiguration
import Network.AWS.ElastiCache.Types.OutpostMode
import Network.AWS.ElastiCache.Types.Parameter
import Network.AWS.ElastiCache.Types.ParameterNameValue
import Network.AWS.ElastiCache.Types.PendingAutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.PendingModifiedValues
import Network.AWS.ElastiCache.Types.ProcessedUpdateAction
import Network.AWS.ElastiCache.Types.RecurringCharge
import Network.AWS.ElastiCache.Types.RegionalConfiguration
import Network.AWS.ElastiCache.Types.ReplicationGroup
import Network.AWS.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import Network.AWS.ElastiCache.Types.ReservedCacheNode
import Network.AWS.ElastiCache.Types.ReservedCacheNodesOffering
import Network.AWS.ElastiCache.Types.ReshardingConfiguration
import Network.AWS.ElastiCache.Types.ReshardingStatus
import Network.AWS.ElastiCache.Types.SecurityGroupMembership
import Network.AWS.ElastiCache.Types.ServiceUpdate
import Network.AWS.ElastiCache.Types.ServiceUpdateSeverity
import Network.AWS.ElastiCache.Types.ServiceUpdateStatus
import Network.AWS.ElastiCache.Types.ServiceUpdateType
import Network.AWS.ElastiCache.Types.SlaMet
import Network.AWS.ElastiCache.Types.SlotMigration
import Network.AWS.ElastiCache.Types.Snapshot
import Network.AWS.ElastiCache.Types.SourceType
import Network.AWS.ElastiCache.Types.Subnet
import Network.AWS.ElastiCache.Types.SubnetOutpost
import Network.AWS.ElastiCache.Types.Tag
import Network.AWS.ElastiCache.Types.TagListMessage
import Network.AWS.ElastiCache.Types.TimeRangeFilter
import Network.AWS.ElastiCache.Types.UnprocessedUpdateAction
import Network.AWS.ElastiCache.Types.UpdateAction
import Network.AWS.ElastiCache.Types.UpdateActionResultsMessage
import Network.AWS.ElastiCache.Types.UpdateActionStatus
import Network.AWS.ElastiCache.Types.User
import Network.AWS.ElastiCache.Types.UserGroup
import Network.AWS.ElastiCache.Types.UserGroupPendingChanges
import Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-02-02@ of the Amazon ElastiCache SDK configuration.
elastiCacheService :: Lude.Service
elastiCacheService =
  Lude.Service
    { Lude._svcAbbrev = "ElastiCache",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "elasticache",
      Lude._svcVersion = "2015-02-02",
      Lude._svcEndpoint = Lude.defaultEndpoint elastiCacheService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseXMLError "ElastiCache",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
