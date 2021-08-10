{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _SubnetInUse,
    _CacheSubnetGroupQuotaExceededFault,
    _InvalidVPCNetworkStateFault,
    _CacheClusterNotFoundFault,
    _InvalidReplicationGroupStateFault,
    _SubnetNotAllowedFault,
    _CacheSubnetGroupAlreadyExistsFault,
    _TestFailoverNotAvailableFault,
    _GlobalReplicationGroupAlreadyExistsFault,
    _SnapshotFeatureNotSupportedFault,
    _UserGroupQuotaExceededFault,
    _InvalidKMSKeyFault,
    _InvalidCacheSecurityGroupStateFault,
    _ReservedCacheNodeNotFoundFault,
    _ServiceLinkedRoleNotFoundFault,
    _APICallRateForCustomerExceededFault,
    _InvalidCacheParameterGroupStateFault,
    _UserQuotaExceededFault,
    _NoOperationFault,
    _InvalidParameterCombinationException,
    _CacheParameterGroupAlreadyExistsFault,
    _SnapshotQuotaExceededFault,
    _TagNotFoundFault,
    _CacheSubnetQuotaExceededFault,
    _SnapshotAlreadyExistsFault,
    _NodeQuotaForCustomerExceededFault,
    _UserNotFoundFault,
    _InsufficientCacheClusterCapacityFault,
    _InvalidUserStateFault,
    _ServiceUpdateNotFoundFault,
    _SnapshotNotFoundFault,
    _UserGroupAlreadyExistsFault,
    _InvalidUserGroupStateFault,
    _InvalidCacheClusterStateFault,
    _UserGroupNotFoundFault,
    _NodeGroupsPerReplicationGroupQuotaExceededFault,
    _ReplicationGroupAlreadyExistsFault,
    _AuthorizationAlreadyExistsFault,
    _ReservedCacheNodeQuotaExceededFault,
    _InvalidParameterValueException,
    _CacheSecurityGroupNotFoundFault,
    _ReservedCacheNodeAlreadyExistsFault,
    _CacheSubnetGroupInUse,
    _InvalidGlobalReplicationGroupStateFault,
    _GlobalReplicationGroupNotFoundFault,
    _CacheSubnetGroupNotFoundFault,
    _CacheSecurityGroupAlreadyExistsFault,
    _InvalidARNFault,
    _NodeGroupNotFoundFault,
    _NodeQuotaForClusterExceededFault,
    _ReplicationGroupAlreadyUnderMigrationFault,
    _DefaultUserRequired,
    _CacheParameterGroupNotFoundFault,
    _DuplicateUserNameFault,
    _InvalidSnapshotStateFault,
    _ReplicationGroupNotUnderMigrationFault,
    _UserAlreadyExistsFault,
    _DefaultUserAssociatedToUserGroupFault,
    _CacheParameterGroupQuotaExceededFault,
    _AuthorizationNotFoundFault,
    _ReservedCacheNodesOfferingNotFoundFault,
    _InvalidSubnet,
    _ClusterQuotaForCustomerExceededFault,
    _ReplicationGroupNotFoundFault,
    _CacheClusterAlreadyExistsFault,
    _TagQuotaPerResourceExceeded,
    _CacheSecurityGroupQuotaExceededFault,

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
    newAuthentication,
    authentication_passwordCount,
    authentication_type,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * CacheCluster
    CacheCluster (..),
    newCacheCluster,
    cacheCluster_cacheNodes,
    cacheCluster_cacheClusterCreateTime,
    cacheCluster_numCacheNodes,
    cacheCluster_cacheSecurityGroups,
    cacheCluster_replicationGroupId,
    cacheCluster_cacheClusterId,
    cacheCluster_snapshotWindow,
    cacheCluster_arn,
    cacheCluster_notificationConfiguration,
    cacheCluster_snapshotRetentionLimit,
    cacheCluster_cacheParameterGroup,
    cacheCluster_securityGroups,
    cacheCluster_preferredAvailabilityZone,
    cacheCluster_atRestEncryptionEnabled,
    cacheCluster_cacheSubnetGroupName,
    cacheCluster_engineVersion,
    cacheCluster_preferredMaintenanceWindow,
    cacheCluster_cacheNodeType,
    cacheCluster_authTokenLastModifiedDate,
    cacheCluster_clientDownloadLandingPage,
    cacheCluster_engine,
    cacheCluster_preferredOutpostArn,
    cacheCluster_authTokenEnabled,
    cacheCluster_pendingModifiedValues,
    cacheCluster_configurationEndpoint,
    cacheCluster_transitEncryptionEnabled,
    cacheCluster_autoMinorVersionUpgrade,
    cacheCluster_cacheClusterStatus,

    -- * CacheEngineVersion
    CacheEngineVersion (..),
    newCacheEngineVersion,
    cacheEngineVersion_cacheEngineDescription,
    cacheEngineVersion_cacheEngineVersionDescription,
    cacheEngineVersion_engineVersion,
    cacheEngineVersion_cacheParameterGroupFamily,
    cacheEngineVersion_engine,

    -- * CacheNode
    CacheNode (..),
    newCacheNode,
    cacheNode_customerAvailabilityZone,
    cacheNode_cacheNodeStatus,
    cacheNode_cacheNodeCreateTime,
    cacheNode_parameterGroupStatus,
    cacheNode_customerOutpostArn,
    cacheNode_sourceCacheNodeId,
    cacheNode_cacheNodeId,
    cacheNode_endpoint,

    -- * CacheNodeTypeSpecificParameter
    CacheNodeTypeSpecificParameter (..),
    newCacheNodeTypeSpecificParameter,
    cacheNodeTypeSpecificParameter_changeType,
    cacheNodeTypeSpecificParameter_allowedValues,
    cacheNodeTypeSpecificParameter_source,
    cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues,
    cacheNodeTypeSpecificParameter_parameterName,
    cacheNodeTypeSpecificParameter_description,
    cacheNodeTypeSpecificParameter_dataType,
    cacheNodeTypeSpecificParameter_isModifiable,
    cacheNodeTypeSpecificParameter_minimumEngineVersion,

    -- * CacheNodeTypeSpecificValue
    CacheNodeTypeSpecificValue (..),
    newCacheNodeTypeSpecificValue,
    cacheNodeTypeSpecificValue_cacheNodeType,
    cacheNodeTypeSpecificValue_value,

    -- * CacheNodeUpdateStatus
    CacheNodeUpdateStatus (..),
    newCacheNodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate,
    cacheNodeUpdateStatus_nodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateInitiatedBy,
    cacheNodeUpdateStatus_nodeUpdateInitiatedDate,
    cacheNodeUpdateStatus_cacheNodeId,
    cacheNodeUpdateStatus_nodeDeletionDate,
    cacheNodeUpdateStatus_nodeUpdateStartDate,
    cacheNodeUpdateStatus_nodeUpdateEndDate,

    -- * CacheParameterGroup
    CacheParameterGroup (..),
    newCacheParameterGroup,
    cacheParameterGroup_isGlobal,
    cacheParameterGroup_arn,
    cacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroup_cacheParameterGroupFamily,
    cacheParameterGroup_description,

    -- * CacheParameterGroupNameMessage
    CacheParameterGroupNameMessage (..),
    newCacheParameterGroupNameMessage,
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- * CacheParameterGroupStatus
    CacheParameterGroupStatus (..),
    newCacheParameterGroupStatus,
    cacheParameterGroupStatus_cacheParameterGroupName,
    cacheParameterGroupStatus_parameterApplyStatus,
    cacheParameterGroupStatus_cacheNodeIdsToReboot,

    -- * CacheSecurityGroup
    CacheSecurityGroup (..),
    newCacheSecurityGroup,
    cacheSecurityGroup_ownerId,
    cacheSecurityGroup_arn,
    cacheSecurityGroup_cacheSecurityGroupName,
    cacheSecurityGroup_eC2SecurityGroups,
    cacheSecurityGroup_description,

    -- * CacheSecurityGroupMembership
    CacheSecurityGroupMembership (..),
    newCacheSecurityGroupMembership,
    cacheSecurityGroupMembership_status,
    cacheSecurityGroupMembership_cacheSecurityGroupName,

    -- * CacheSubnetGroup
    CacheSubnetGroup (..),
    newCacheSubnetGroup,
    cacheSubnetGroup_arn,
    cacheSubnetGroup_cacheSubnetGroupName,
    cacheSubnetGroup_cacheSubnetGroupDescription,
    cacheSubnetGroup_subnets,
    cacheSubnetGroup_vpcId,

    -- * ConfigureShard
    ConfigureShard (..),
    newConfigureShard,
    configureShard_preferredAvailabilityZones,
    configureShard_preferredOutpostArns,
    configureShard_nodeGroupId,
    configureShard_newReplicaCount,

    -- * CustomerNodeEndpoint
    CustomerNodeEndpoint (..),
    newCustomerNodeEndpoint,
    customerNodeEndpoint_address,
    customerNodeEndpoint_port,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_port,

    -- * EngineDefaults
    EngineDefaults (..),
    newEngineDefaults,
    engineDefaults_cacheNodeTypeSpecificParameters,
    engineDefaults_cacheParameterGroupFamily,
    engineDefaults_parameters,
    engineDefaults_marker,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_date,
    event_sourceIdentifier,
    event_sourceType,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * GlobalNodeGroup
    GlobalNodeGroup (..),
    newGlobalNodeGroup,
    globalNodeGroup_globalNodeGroupId,
    globalNodeGroup_slots,

    -- * GlobalReplicationGroup
    GlobalReplicationGroup (..),
    newGlobalReplicationGroup,
    globalReplicationGroup_clusterEnabled,
    globalReplicationGroup_status,
    globalReplicationGroup_arn,
    globalReplicationGroup_globalReplicationGroupId,
    globalReplicationGroup_atRestEncryptionEnabled,
    globalReplicationGroup_globalNodeGroups,
    globalReplicationGroup_engineVersion,
    globalReplicationGroup_cacheNodeType,
    globalReplicationGroup_globalReplicationGroupDescription,
    globalReplicationGroup_engine,
    globalReplicationGroup_authTokenEnabled,
    globalReplicationGroup_members,
    globalReplicationGroup_transitEncryptionEnabled,

    -- * GlobalReplicationGroupInfo
    GlobalReplicationGroupInfo (..),
    newGlobalReplicationGroupInfo,
    globalReplicationGroupInfo_globalReplicationGroupMemberRole,
    globalReplicationGroupInfo_globalReplicationGroupId,

    -- * GlobalReplicationGroupMember
    GlobalReplicationGroupMember (..),
    newGlobalReplicationGroupMember,
    globalReplicationGroupMember_status,
    globalReplicationGroupMember_automaticFailover,
    globalReplicationGroupMember_replicationGroupId,
    globalReplicationGroupMember_replicationGroupRegion,
    globalReplicationGroupMember_role,

    -- * NodeGroup
    NodeGroup (..),
    newNodeGroup,
    nodeGroup_status,
    nodeGroup_readerEndpoint,
    nodeGroup_nodeGroupId,
    nodeGroup_primaryEndpoint,
    nodeGroup_slots,
    nodeGroup_nodeGroupMembers,

    -- * NodeGroupConfiguration
    NodeGroupConfiguration (..),
    newNodeGroupConfiguration,
    nodeGroupConfiguration_primaryOutpostArn,
    nodeGroupConfiguration_replicaCount,
    nodeGroupConfiguration_replicaOutpostArns,
    nodeGroupConfiguration_nodeGroupId,
    nodeGroupConfiguration_slots,
    nodeGroupConfiguration_replicaAvailabilityZones,
    nodeGroupConfiguration_primaryAvailabilityZone,

    -- * NodeGroupMember
    NodeGroupMember (..),
    newNodeGroupMember,
    nodeGroupMember_cacheClusterId,
    nodeGroupMember_preferredAvailabilityZone,
    nodeGroupMember_readEndpoint,
    nodeGroupMember_cacheNodeId,
    nodeGroupMember_preferredOutpostArn,
    nodeGroupMember_currentRole,

    -- * NodeGroupMemberUpdateStatus
    NodeGroupMemberUpdateStatus (..),
    newNodeGroupMemberUpdateStatus,
    nodeGroupMemberUpdateStatus_cacheClusterId,
    nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStatus,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate,
    nodeGroupMemberUpdateStatus_cacheNodeId,
    nodeGroupMemberUpdateStatus_nodeDeletionDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStartDate,
    nodeGroupMemberUpdateStatus_nodeUpdateEndDate,

    -- * NodeGroupUpdateStatus
    NodeGroupUpdateStatus (..),
    newNodeGroupUpdateStatus,
    nodeGroupUpdateStatus_nodeGroupId,
    nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus,

    -- * NodeSnapshot
    NodeSnapshot (..),
    newNodeSnapshot,
    nodeSnapshot_nodeGroupConfiguration,
    nodeSnapshot_cacheSize,
    nodeSnapshot_cacheClusterId,
    nodeSnapshot_snapshotCreateTime,
    nodeSnapshot_cacheNodeCreateTime,
    nodeSnapshot_nodeGroupId,
    nodeSnapshot_cacheNodeId,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_topicStatus,
    notificationConfiguration_topicArn,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_changeType,
    parameter_allowedValues,
    parameter_source,
    parameter_parameterValue,
    parameter_parameterName,
    parameter_description,
    parameter_dataType,
    parameter_isModifiable,
    parameter_minimumEngineVersion,

    -- * ParameterNameValue
    ParameterNameValue (..),
    newParameterNameValue,
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
    pendingModifiedValues_numCacheNodes,
    pendingModifiedValues_cacheNodeIdsToRemove,
    pendingModifiedValues_authTokenStatus,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_cacheNodeType,

    -- * ProcessedUpdateAction
    ProcessedUpdateAction (..),
    newProcessedUpdateAction,
    processedUpdateAction_replicationGroupId,
    processedUpdateAction_updateActionStatus,
    processedUpdateAction_cacheClusterId,
    processedUpdateAction_serviceUpdateName,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- * RegionalConfiguration
    RegionalConfiguration (..),
    newRegionalConfiguration,
    regionalConfiguration_replicationGroupId,
    regionalConfiguration_replicationGroupRegion,
    regionalConfiguration_reshardingConfiguration,

    -- * ReplicationGroup
    ReplicationGroup (..),
    newReplicationGroup,
    replicationGroup_clusterEnabled,
    replicationGroup_status,
    replicationGroup_nodeGroups,
    replicationGroup_automaticFailover,
    replicationGroup_memberClustersOutpostArns,
    replicationGroup_memberClusters,
    replicationGroup_globalReplicationGroupInfo,
    replicationGroup_replicationGroupId,
    replicationGroup_userGroupIds,
    replicationGroup_snapshotWindow,
    replicationGroup_arn,
    replicationGroup_snapshotRetentionLimit,
    replicationGroup_multiAZ,
    replicationGroup_atRestEncryptionEnabled,
    replicationGroup_kmsKeyId,
    replicationGroup_snapshottingClusterId,
    replicationGroup_cacheNodeType,
    replicationGroup_authTokenLastModifiedDate,
    replicationGroup_authTokenEnabled,
    replicationGroup_description,
    replicationGroup_pendingModifiedValues,
    replicationGroup_configurationEndpoint,
    replicationGroup_transitEncryptionEnabled,

    -- * ReplicationGroupPendingModifiedValues
    ReplicationGroupPendingModifiedValues (..),
    newReplicationGroupPendingModifiedValues,
    replicationGroupPendingModifiedValues_resharding,
    replicationGroupPendingModifiedValues_primaryClusterId,
    replicationGroupPendingModifiedValues_authTokenStatus,
    replicationGroupPendingModifiedValues_userGroups,
    replicationGroupPendingModifiedValues_automaticFailoverStatus,

    -- * ReservedCacheNode
    ReservedCacheNode (..),
    newReservedCacheNode,
    reservedCacheNode_reservedCacheNodesOfferingId,
    reservedCacheNode_duration,
    reservedCacheNode_startTime,
    reservedCacheNode_state,
    reservedCacheNode_cacheNodeCount,
    reservedCacheNode_cacheNodeType,
    reservedCacheNode_fixedPrice,
    reservedCacheNode_usagePrice,
    reservedCacheNode_offeringType,
    reservedCacheNode_recurringCharges,
    reservedCacheNode_productDescription,
    reservedCacheNode_reservedCacheNodeId,
    reservedCacheNode_reservationARN,

    -- * ReservedCacheNodesOffering
    ReservedCacheNodesOffering (..),
    newReservedCacheNodesOffering,
    reservedCacheNodesOffering_reservedCacheNodesOfferingId,
    reservedCacheNodesOffering_duration,
    reservedCacheNodesOffering_cacheNodeType,
    reservedCacheNodesOffering_fixedPrice,
    reservedCacheNodesOffering_usagePrice,
    reservedCacheNodesOffering_offeringType,
    reservedCacheNodesOffering_recurringCharges,
    reservedCacheNodesOffering_productDescription,

    -- * ReshardingConfiguration
    ReshardingConfiguration (..),
    newReshardingConfiguration,
    reshardingConfiguration_preferredAvailabilityZones,
    reshardingConfiguration_nodeGroupId,

    -- * ReshardingStatus
    ReshardingStatus (..),
    newReshardingStatus,
    reshardingStatus_slotMigration,

    -- * SecurityGroupMembership
    SecurityGroupMembership (..),
    newSecurityGroupMembership,
    securityGroupMembership_status,
    securityGroupMembership_securityGroupId,

    -- * ServiceUpdate
    ServiceUpdate (..),
    newServiceUpdate,
    serviceUpdate_serviceUpdateSeverity,
    serviceUpdate_autoUpdateAfterRecommendedApplyByDate,
    serviceUpdate_serviceUpdateReleaseDate,
    serviceUpdate_serviceUpdateStatus,
    serviceUpdate_serviceUpdateRecommendedApplyByDate,
    serviceUpdate_serviceUpdateEndDate,
    serviceUpdate_engineVersion,
    serviceUpdate_serviceUpdateType,
    serviceUpdate_estimatedUpdateTime,
    serviceUpdate_engine,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_serviceUpdateDescription,

    -- * SlotMigration
    SlotMigration (..),
    newSlotMigration,
    slotMigration_progressPercentage,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_cacheClusterCreateTime,
    snapshot_nodeSnapshots,
    snapshot_automaticFailover,
    snapshot_numCacheNodes,
    snapshot_replicationGroupId,
    snapshot_cacheClusterId,
    snapshot_snapshotWindow,
    snapshot_snapshotStatus,
    snapshot_arn,
    snapshot_replicationGroupDescription,
    snapshot_cacheParameterGroupName,
    snapshot_snapshotRetentionLimit,
    snapshot_numNodeGroups,
    snapshot_preferredAvailabilityZone,
    snapshot_kmsKeyId,
    snapshot_cacheSubnetGroupName,
    snapshot_engineVersion,
    snapshot_preferredMaintenanceWindow,
    snapshot_cacheNodeType,
    snapshot_topicArn,
    snapshot_port,
    snapshot_snapshotSource,
    snapshot_engine,
    snapshot_preferredOutpostArn,
    snapshot_snapshotName,
    snapshot_vpcId,
    snapshot_autoMinorVersionUpgrade,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,
    subnet_subnetOutpost,

    -- * SubnetOutpost
    SubnetOutpost (..),
    newSubnetOutpost,
    subnetOutpost_subnetOutpostArn,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TagListMessage
    TagListMessage (..),
    newTagListMessage,
    tagListMessage_tagList,

    -- * TimeRangeFilter
    TimeRangeFilter (..),
    newTimeRangeFilter,
    timeRangeFilter_startTime,
    timeRangeFilter_endTime,

    -- * UnprocessedUpdateAction
    UnprocessedUpdateAction (..),
    newUnprocessedUpdateAction,
    unprocessedUpdateAction_replicationGroupId,
    unprocessedUpdateAction_cacheClusterId,
    unprocessedUpdateAction_errorType,
    unprocessedUpdateAction_errorMessage,
    unprocessedUpdateAction_serviceUpdateName,

    -- * UpdateAction
    UpdateAction (..),
    newUpdateAction,
    updateAction_serviceUpdateSeverity,
    updateAction_serviceUpdateReleaseDate,
    updateAction_replicationGroupId,
    updateAction_updateActionStatus,
    updateAction_cacheClusterId,
    updateAction_serviceUpdateStatus,
    updateAction_slaMet,
    updateAction_updateActionAvailableDate,
    updateAction_nodeGroupUpdateStatus,
    updateAction_serviceUpdateRecommendedApplyByDate,
    updateAction_serviceUpdateType,
    updateAction_cacheNodeUpdateStatus,
    updateAction_nodesUpdated,
    updateAction_estimatedUpdateTime,
    updateAction_updateActionStatusModifiedDate,
    updateAction_engine,
    updateAction_serviceUpdateName,

    -- * UpdateActionResultsMessage
    UpdateActionResultsMessage (..),
    newUpdateActionResultsMessage,
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- * User
    User (..),
    newUser,
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,

    -- * UserGroup
    UserGroup (..),
    newUserGroup,
    userGroup_status,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_userIds,
    userGroup_engine,
    userGroup_userGroupId,
    userGroup_pendingChanges,

    -- * UserGroupPendingChanges
    UserGroupPendingChanges (..),
    newUserGroupPendingChanges,
    userGroupPendingChanges_userIdsToRemove,
    userGroupPendingChanges_userIdsToAdd,

    -- * UserGroupsUpdateStatus
    UserGroupsUpdateStatus (..),
    newUserGroupsUpdateStatus,
    userGroupsUpdateStatus_userGroupIdsToRemove,
    userGroupsUpdateStatus_userGroupIdsToAdd,
  )
where

import qualified Network.AWS.Core as Core
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-02-02@ of the Amazon ElastiCache SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "ElastiCache",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "elasticache",
      Core._serviceSigningName = "elasticache",
      Core._serviceVersion = "2015-02-02",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseXMLError "ElastiCache",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The requested subnet is being used by another cache subnet group.
_SubnetInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError
    defaultService
    "SubnetInUse"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache subnet groups.
_CacheSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The requested cluster ID does not refer to an existing cluster.
_CacheClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheClusterNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested replication group is not in the @available@ state.
_InvalidReplicationGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReplicationGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidReplicationGroupState"
    Prelude.. Core.hasStatus 400

-- | At least one subnet ID does not match the other subnet IDs. This
-- mismatch typically occurs when a user sets one subnet ID to a regional
-- Availability Zone and a different one to an outpost. Or when a user sets
-- the subnet ID to an Outpost when not subscribed on this service.
_SubnetNotAllowedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "SubnetNotAllowedFault"
    Prelude.. Core.hasStatus 400

-- | The requested cache subnet group name is already in use by an existing
-- cache subnet group.
_CacheSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The @TestFailover@ action is not available.
_TestFailoverNotAvailableFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TestFailoverNotAvailableFault =
  Core._MatchServiceError
    defaultService
    "TestFailoverNotAvailableFault"
    Prelude.. Core.hasStatus 400

-- | The Global Datastore name already exists.
_GlobalReplicationGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalReplicationGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalReplicationGroupAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | You attempted one of the following operations:
--
-- -   Creating a snapshot of a Redis cluster running on a @cache.t1.micro@
--     cache node.
--
-- -   Creating a snapshot of a cluster that is running Memcached rather
--     than Redis.
--
-- Neither of these are supported by ElastiCache.
_SnapshotFeatureNotSupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotFeatureNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "SnapshotFeatureNotSupportedFault"
    Prelude.. Core.hasStatus 400

-- | The number of users exceeds the user group limit.
_UserGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The KMS key supplied is not valid.
_InvalidKMSKeyFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKMSKeyFault =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyFault"
    Prelude.. Core.hasStatus 400

-- | The current state of the cache security group does not allow deletion.
_InvalidCacheSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCacheSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The requested reserved cache node was not found.
_ReservedCacheNodeNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The customer has exceeded the allowed rate of API calls.
_APICallRateForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_APICallRateForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "APICallRateForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The current state of the cache parameter group does not allow the
-- requested operation to occur.
_InvalidCacheParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCacheParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The quota of users has been exceeded.
_UserQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The operation was not performed because no changes were required.
_NoOperationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoOperationFault =
  Core._MatchServiceError
    defaultService
    "NoOperationFault"
    Prelude.. Core.hasStatus 400

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombination"
    Prelude.. Core.hasStatus 400

-- | A cache parameter group with the requested name already exists.
_CacheParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the maximum
-- number of snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The requested tag was not found on this resource.
_TagNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFound"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a cache subnet group.
_CacheSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | You already have a snapshot with the given name.
_SnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes per customer.
_NodeQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The user does not exist or could not be found.
_UserNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested cache node type is not available in the specified
-- Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ErrorMessages.html#ErrorMessages.INSUFFICIENT_CACHE_CLUSTER_CAPACITY InsufficientCacheClusterCapacity>
-- in the ElastiCache User Guide.
_InsufficientCacheClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientCacheClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientCacheClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The user is not in active state.
_InvalidUserStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidUserState"
    Prelude.. Core.hasStatus 400

-- | The service update doesn\'t exist
_ServiceUpdateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUpdateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceUpdateNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The requested snapshot name does not refer to an existing snapshot.
_SnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The user group with this ID already exists.
_UserGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The user group is not in an active state.
_InvalidUserGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidUserGroupState"
    Prelude.. Core.hasStatus 400

-- | The requested cluster is not in the @available@ state.
_InvalidCacheClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCacheClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheClusterState"
    Prelude.. Core.hasStatus 400

-- | The user group was not found or does not exist
_UserGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the maximum
-- allowed number of node groups (shards) in a single replication group.
-- The default maximum is 90
_NodeGroupsPerReplicationGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeGroupsPerReplicationGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeGroupsPerReplicationGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified replication group already exists.
_ReplicationGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon EC2 security group is already authorized for the
-- specified cache security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the user\'s
-- cache node quota.
_ReservedCacheNodeQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Prelude.. Core.hasStatus 400

-- | The requested cache security group name does not refer to an existing
-- cache security group.
_CacheSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | You already have a reservation with the given identifier.
_ReservedCacheNodeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | The requested cache subnet group is currently in use.
_CacheSubnetGroupInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupInUse =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupInUse"
    Prelude.. Core.hasStatus 400

-- | The Global Datastore is not available or in primary-only state.
_InvalidGlobalReplicationGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalReplicationGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalReplicationGroupState"
    Prelude.. Core.hasStatus 400

-- | The Global Datastore does not exist
_GlobalReplicationGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalReplicationGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalReplicationGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The requested cache subnet group name does not refer to an existing
-- cache subnet group.
_CacheSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | A cache security group with the specified name already exists.
_CacheSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidARNFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError defaultService "InvalidARN"
    Prelude.. Core.hasStatus 400

-- | The node group specified by the @NodeGroupId@ parameter could not be
-- found. Please verify that the node group exists and that you spelled the
-- @NodeGroupId@ value correctly.
_NodeGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "NodeGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes in a single cluster.
_NodeQuotaForClusterExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceeded"
    Prelude.. Core.hasStatus 400

-- | The targeted replication group is not available.
_ReplicationGroupAlreadyUnderMigrationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupAlreadyUnderMigrationFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupAlreadyUnderMigrationFault"
    Prelude.. Core.hasStatus 400

-- | You must add default user to a user group.
_DefaultUserRequired :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultUserRequired =
  Core._MatchServiceError
    defaultService
    "DefaultUserRequired"
    Prelude.. Core.hasStatus 400

-- | The requested cache parameter group name does not refer to an existing
-- cache parameter group.
_CacheParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | A user with this username already exists.
_DuplicateUserNameFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateUserNameFault =
  Core._MatchServiceError
    defaultService
    "DuplicateUserName"
    Prelude.. Core.hasStatus 400

-- | The current state of the snapshot does not allow the requested operation
-- to occur.
_InvalidSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The designated replication group is not available for data migration.
_ReplicationGroupNotUnderMigrationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupNotUnderMigrationFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupNotUnderMigrationFault"
    Prelude.. Core.hasStatus 400

-- | A user with this ID already exists.
_UserAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserAlreadyExists"
    Prelude.. Core.hasStatus 400

-- |
_DefaultUserAssociatedToUserGroupFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultUserAssociatedToUserGroupFault =
  Core._MatchServiceError
    defaultService
    "DefaultUserAssociatedToUserGroup"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the maximum
-- number of cache security groups.
_CacheParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon EC2 security group is not authorized for the
-- specified cache security group.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested cache node offering does not exist.
_ReservedCacheNodesOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodesOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of clusters per customer.
_ClusterQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified replication group does not exist.
_ReplicationGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You already have a cluster with the given identifier.
_CacheClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheClusterAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would cause the resource to
-- have more than the allowed number of tags. The maximum number of tags
-- permitted on a resource is 50.
_TagQuotaPerResourceExceeded :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache security groups.
_CacheSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.CacheSecurityGroup"
    Prelude.. Core.hasStatus 400
