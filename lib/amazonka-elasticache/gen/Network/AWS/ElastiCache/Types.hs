{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types
  ( -- * Service Configuration
    elastiCache,

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
    Authentication,
    authentication,
    aPasswordCount,
    aType,

    -- * AvailabilityZone
    AvailabilityZone,
    availabilityZone,
    azName,

    -- * CacheCluster
    CacheCluster,
    cacheCluster,
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
    CacheEngineVersion,
    cacheEngineVersion,
    cevEngineVersion,
    cevCacheParameterGroupFamily,
    cevCacheEngineDescription,
    cevEngine,
    cevCacheEngineVersionDescription,

    -- * CacheNode
    CacheNode,
    cacheNode,
    cnSourceCacheNodeId,
    cnParameterGroupStatus,
    cnCacheNodeCreateTime,
    cnCustomerAvailabilityZone,
    cnCacheNodeId,
    cnCustomerOutpostARN,
    cnCacheNodeStatus,
    cnEndpoint,

    -- * CacheNodeTypeSpecificParameter
    CacheNodeTypeSpecificParameter,
    cacheNodeTypeSpecificParameter,
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
    CacheNodeTypeSpecificValue,
    cacheNodeTypeSpecificValue,
    cntsvCacheNodeType,
    cntsvValue,

    -- * CacheNodeUpdateStatus
    CacheNodeUpdateStatus,
    cacheNodeUpdateStatus,
    cnusNodeUpdateEndDate,
    cnusNodeUpdateInitiatedBy,
    cnusNodeUpdateStatusModifiedDate,
    cnusCacheNodeId,
    cnusNodeUpdateInitiatedDate,
    cnusNodeUpdateStartDate,
    cnusNodeUpdateStatus,
    cnusNodeDeletionDate,

    -- * CacheParameterGroup
    CacheParameterGroup,
    cacheParameterGroup,
    cpgCacheParameterGroupFamily,
    cpgARN,
    cpgCacheParameterGroupName,
    cpgIsGlobal,
    cpgDescription,

    -- * CacheParameterGroupNameMessage
    CacheParameterGroupNameMessage,
    cacheParameterGroupNameMessage,
    cpgnmCacheParameterGroupName,

    -- * CacheParameterGroupStatus
    CacheParameterGroupStatus,
    cacheParameterGroupStatus,
    cpgsCacheParameterGroupName,
    cpgsCacheNodeIdsToReboot,
    cpgsParameterApplyStatus,

    -- * CacheSecurityGroup
    CacheSecurityGroup,
    cacheSecurityGroup,
    csgCacheSecurityGroupName,
    csgARN,
    csgOwnerId,
    csgEC2SecurityGroups,
    csgDescription,

    -- * CacheSecurityGroupMembership
    CacheSecurityGroupMembership,
    cacheSecurityGroupMembership,
    csgmStatus,
    csgmCacheSecurityGroupName,

    -- * CacheSubnetGroup
    CacheSubnetGroup,
    cacheSubnetGroup,
    cARN,
    cVPCId,
    cSubnets,
    cCacheSubnetGroupName,
    cCacheSubnetGroupDescription,

    -- * ConfigureShard
    ConfigureShard,
    configureShard,
    csPreferredAvailabilityZones,
    csPreferredOutpostARNs,
    csNodeGroupId,
    csNewReplicaCount,

    -- * CustomerNodeEndpoint
    CustomerNodeEndpoint,
    customerNodeEndpoint,
    cneAddress,
    cnePort,

    -- * EC2SecurityGroup
    EC2SecurityGroup,
    ec2SecurityGroup,
    esgStatus,
    esgEC2SecurityGroupOwnerId,
    esgEC2SecurityGroupName,

    -- * Endpoint
    Endpoint,
    endpoint,
    eAddress,
    ePort,

    -- * EngineDefaults
    EngineDefaults,
    engineDefaults,
    edCacheParameterGroupFamily,
    edCacheNodeTypeSpecificParameters,
    edMarker,
    edParameters,

    -- * Event
    Event,
    event,
    eSourceType,
    eSourceIdentifier,
    eDate,
    eMessage,

    -- * Filter
    Filter,
    filter',
    fName,
    fValues,

    -- * GlobalNodeGroup
    GlobalNodeGroup,
    globalNodeGroup,
    gngSlots,
    gngGlobalNodeGroupId,

    -- * GlobalReplicationGroup
    GlobalReplicationGroup,
    globalReplicationGroup,
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
    GlobalReplicationGroupInfo,
    globalReplicationGroupInfo,
    grgiGlobalReplicationGroupMemberRole,
    grgiGlobalReplicationGroupId,

    -- * GlobalReplicationGroupMember
    GlobalReplicationGroupMember,
    globalReplicationGroupMember,
    grgmStatus,
    grgmReplicationGroupRegion,
    grgmRole,
    grgmReplicationGroupId,
    grgmAutomaticFailover,

    -- * NodeGroup
    NodeGroup,
    nodeGroup,
    ngStatus,
    ngPrimaryEndpoint,
    ngSlots,
    ngNodeGroupMembers,
    ngNodeGroupId,
    ngReaderEndpoint,

    -- * NodeGroupConfiguration
    NodeGroupConfiguration,
    nodeGroupConfiguration,
    ngcSlots,
    ngcReplicaOutpostARNs,
    ngcReplicaCount,
    ngcPrimaryAvailabilityZone,
    ngcReplicaAvailabilityZones,
    ngcPrimaryOutpostARN,
    ngcNodeGroupId,

    -- * NodeGroupMember
    NodeGroupMember,
    nodeGroupMember,
    ngmCacheClusterId,
    ngmCacheNodeId,
    ngmPreferredAvailabilityZone,
    ngmCurrentRole,
    ngmPreferredOutpostARN,
    ngmReadEndpoint,

    -- * NodeGroupMemberUpdateStatus
    NodeGroupMemberUpdateStatus,
    nodeGroupMemberUpdateStatus,
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
    NodeGroupUpdateStatus,
    nodeGroupUpdateStatus,
    ngusNodeGroupMemberUpdateStatus,
    ngusNodeGroupId,

    -- * NodeSnapshot
    NodeSnapshot,
    nodeSnapshot,
    nsNodeGroupConfiguration,
    nsCacheNodeCreateTime,
    nsCacheClusterId,
    nsCacheNodeId,
    nsNodeGroupId,
    nsSnapshotCreateTime,
    nsCacheSize,

    -- * NotificationConfiguration
    NotificationConfiguration,
    notificationConfiguration,
    ncTopicStatus,
    ncTopicARN,

    -- * Parameter
    Parameter,
    parameter,
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
    ParameterNameValue,
    parameterNameValue,
    pnvParameterValue,
    pnvParameterName,

    -- * PendingModifiedValues
    PendingModifiedValues,
    pendingModifiedValues,
    pmvEngineVersion,
    pmvCacheNodeType,
    pmvAuthTokenStatus,
    pmvCacheNodeIdsToRemove,
    pmvNumCacheNodes,

    -- * ProcessedUpdateAction
    ProcessedUpdateAction,
    processedUpdateAction,
    puaCacheClusterId,
    puaServiceUpdateName,
    puaUpdateActionStatus,
    puaReplicationGroupId,

    -- * RecurringCharge
    RecurringCharge,
    recurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * RegionalConfiguration
    RegionalConfiguration,
    regionalConfiguration,
    rcReplicationGroupId,
    rcReplicationGroupRegion,
    rcReshardingConfiguration,

    -- * ReplicationGroup
    ReplicationGroup,
    replicationGroup,
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
    ReplicationGroupPendingModifiedValues,
    replicationGroupPendingModifiedValues,
    rgpmvAuthTokenStatus,
    rgpmvUserGroups,
    rgpmvResharding,
    rgpmvPrimaryClusterId,
    rgpmvAutomaticFailoverStatus,

    -- * ReservedCacheNode
    ReservedCacheNode,
    reservedCacheNode,
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
    ReservedCacheNodesOffering,
    reservedCacheNodesOffering,
    rcnoCacheNodeType,
    rcnoProductDescription,
    rcnoRecurringCharges,
    rcnoOfferingType,
    rcnoUsagePrice,
    rcnoFixedPrice,
    rcnoDuration,
    rcnoReservedCacheNodesOfferingId,

    -- * ReshardingConfiguration
    ReshardingConfiguration,
    reshardingConfiguration,
    rcPreferredAvailabilityZones,
    rcNodeGroupId,

    -- * ReshardingStatus
    ReshardingStatus,
    reshardingStatus,
    rsSlotMigration,

    -- * SecurityGroupMembership
    SecurityGroupMembership,
    securityGroupMembership,
    sgmStatus,
    sgmSecurityGroupId,

    -- * ServiceUpdate
    ServiceUpdate,
    serviceUpdate,
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
    SlotMigration,
    slotMigration,
    smProgressPercentage,

    -- * Snapshot
    Snapshot,
    snapshot,
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
    Subnet,
    subnet,
    sSubnetIdentifier,
    sSubnetAvailabilityZone,
    sSubnetOutpost,

    -- * SubnetOutpost
    SubnetOutpost,
    subnetOutpost,
    soSubnetOutpostARN,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * TagListMessage
    TagListMessage,
    tagListMessage,
    tlmTagList,

    -- * TimeRangeFilter
    TimeRangeFilter,
    timeRangeFilter,
    trfStartTime,
    trfEndTime,

    -- * UnprocessedUpdateAction
    UnprocessedUpdateAction,
    unprocessedUpdateAction,
    uuaCacheClusterId,
    uuaServiceUpdateName,
    uuaErrorType,
    uuaErrorMessage,
    uuaReplicationGroupId,

    -- * UpdateAction
    UpdateAction,
    updateAction,
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
    UpdateActionResultsMessage,
    updateActionResultsMessage,
    uarmUnprocessedUpdateActions,
    uarmProcessedUpdateActions,

    -- * User
    User,
    user,
    uStatus,
    uARN,
    uUserGroupIds,
    uAuthentication,
    uEngine,
    uUserName,
    uAccessString,
    uUserId,

    -- * UserGroup
    UserGroup,
    userGroup,
    ugStatus,
    ugUserIds,
    ugARN,
    ugUserGroupId,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,

    -- * UserGroupPendingChanges
    UserGroupPendingChanges,
    userGroupPendingChanges,
    ugpcUserIdsToAdd,
    ugpcUserIdsToRemove,

    -- * UserGroupsUpdateStatus
    UserGroupsUpdateStatus,
    userGroupsUpdateStatus,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-02-02@ of the Amazon ElastiCache SDK configuration.
elastiCache :: Service
elastiCache =
  Service
    { _svcAbbrev = "ElastiCache",
      _svcSigner = v4,
      _svcPrefix = "elasticache",
      _svcVersion = "2015-02-02",
      _svcEndpoint = defaultEndpoint elastiCache,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseXMLError "ElastiCache",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
