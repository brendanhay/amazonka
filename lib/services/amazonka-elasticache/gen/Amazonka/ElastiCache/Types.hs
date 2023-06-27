{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElastiCache.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _APICallRateForCustomerExceededFault,
    _AuthorizationAlreadyExistsFault,
    _AuthorizationNotFoundFault,
    _CacheClusterAlreadyExistsFault,
    _CacheClusterNotFoundFault,
    _CacheParameterGroupAlreadyExistsFault,
    _CacheParameterGroupNotFoundFault,
    _CacheParameterGroupQuotaExceededFault,
    _CacheSecurityGroupAlreadyExistsFault,
    _CacheSecurityGroupNotFoundFault,
    _CacheSecurityGroupQuotaExceededFault,
    _CacheSubnetGroupAlreadyExistsFault,
    _CacheSubnetGroupInUse,
    _CacheSubnetGroupNotFoundFault,
    _CacheSubnetGroupQuotaExceededFault,
    _CacheSubnetQuotaExceededFault,
    _ClusterQuotaForCustomerExceededFault,
    _DefaultUserAssociatedToUserGroupFault,
    _DefaultUserRequired,
    _DuplicateUserNameFault,
    _GlobalReplicationGroupAlreadyExistsFault,
    _GlobalReplicationGroupNotFoundFault,
    _InsufficientCacheClusterCapacityFault,
    _InvalidARNFault,
    _InvalidCacheClusterStateFault,
    _InvalidCacheParameterGroupStateFault,
    _InvalidCacheSecurityGroupStateFault,
    _InvalidGlobalReplicationGroupStateFault,
    _InvalidKMSKeyFault,
    _InvalidParameterCombinationException,
    _InvalidParameterValueException,
    _InvalidReplicationGroupStateFault,
    _InvalidSnapshotStateFault,
    _InvalidSubnet,
    _InvalidUserGroupStateFault,
    _InvalidUserStateFault,
    _InvalidVPCNetworkStateFault,
    _NoOperationFault,
    _NodeGroupNotFoundFault,
    _NodeGroupsPerReplicationGroupQuotaExceededFault,
    _NodeQuotaForClusterExceededFault,
    _NodeQuotaForCustomerExceededFault,
    _ReplicationGroupAlreadyExistsFault,
    _ReplicationGroupAlreadyUnderMigrationFault,
    _ReplicationGroupNotFoundFault,
    _ReplicationGroupNotUnderMigrationFault,
    _ReservedCacheNodeAlreadyExistsFault,
    _ReservedCacheNodeNotFoundFault,
    _ReservedCacheNodeQuotaExceededFault,
    _ReservedCacheNodesOfferingNotFoundFault,
    _ServiceLinkedRoleNotFoundFault,
    _ServiceUpdateNotFoundFault,
    _SnapshotAlreadyExistsFault,
    _SnapshotFeatureNotSupportedFault,
    _SnapshotNotFoundFault,
    _SnapshotQuotaExceededFault,
    _SubnetInUse,
    _SubnetNotAllowedFault,
    _TagNotFoundFault,
    _TagQuotaPerResourceExceeded,
    _TestFailoverNotAvailableFault,
    _UserAlreadyExistsFault,
    _UserGroupAlreadyExistsFault,
    _UserGroupNotFoundFault,
    _UserGroupQuotaExceededFault,
    _UserNotFoundFault,
    _UserQuotaExceededFault,

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

    -- * ClusterMode
    ClusterMode (..),

    -- * DataTieringStatus
    DataTieringStatus (..),

    -- * DestinationType
    DestinationType (..),

    -- * InputAuthenticationType
    InputAuthenticationType (..),

    -- * IpDiscovery
    IpDiscovery (..),

    -- * LogDeliveryConfigurationStatus
    LogDeliveryConfigurationStatus (..),

    -- * LogFormat
    LogFormat (..),

    -- * LogType
    LogType (..),

    -- * MultiAZStatus
    MultiAZStatus (..),

    -- * NetworkType
    NetworkType (..),

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

    -- * TransitEncryptionMode
    TransitEncryptionMode (..),

    -- * UpdateActionStatus
    UpdateActionStatus (..),

    -- * Authentication
    Authentication (..),
    newAuthentication,
    authentication_passwordCount,
    authentication_type,

    -- * AuthenticationMode
    AuthenticationMode (..),
    newAuthenticationMode,
    authenticationMode_passwords,
    authenticationMode_type,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * CacheCluster
    CacheCluster (..),
    newCacheCluster,
    cacheCluster_arn,
    cacheCluster_atRestEncryptionEnabled,
    cacheCluster_authTokenEnabled,
    cacheCluster_authTokenLastModifiedDate,
    cacheCluster_autoMinorVersionUpgrade,
    cacheCluster_cacheClusterCreateTime,
    cacheCluster_cacheClusterId,
    cacheCluster_cacheClusterStatus,
    cacheCluster_cacheNodeType,
    cacheCluster_cacheNodes,
    cacheCluster_cacheParameterGroup,
    cacheCluster_cacheSecurityGroups,
    cacheCluster_cacheSubnetGroupName,
    cacheCluster_clientDownloadLandingPage,
    cacheCluster_configurationEndpoint,
    cacheCluster_engine,
    cacheCluster_engineVersion,
    cacheCluster_ipDiscovery,
    cacheCluster_logDeliveryConfigurations,
    cacheCluster_networkType,
    cacheCluster_notificationConfiguration,
    cacheCluster_numCacheNodes,
    cacheCluster_pendingModifiedValues,
    cacheCluster_preferredAvailabilityZone,
    cacheCluster_preferredMaintenanceWindow,
    cacheCluster_preferredOutpostArn,
    cacheCluster_replicationGroupId,
    cacheCluster_replicationGroupLogDeliveryEnabled,
    cacheCluster_securityGroups,
    cacheCluster_snapshotRetentionLimit,
    cacheCluster_snapshotWindow,
    cacheCluster_transitEncryptionEnabled,
    cacheCluster_transitEncryptionMode,

    -- * CacheEngineVersion
    CacheEngineVersion (..),
    newCacheEngineVersion,
    cacheEngineVersion_cacheEngineDescription,
    cacheEngineVersion_cacheEngineVersionDescription,
    cacheEngineVersion_cacheParameterGroupFamily,
    cacheEngineVersion_engine,
    cacheEngineVersion_engineVersion,

    -- * CacheNode
    CacheNode (..),
    newCacheNode,
    cacheNode_cacheNodeCreateTime,
    cacheNode_cacheNodeId,
    cacheNode_cacheNodeStatus,
    cacheNode_customerAvailabilityZone,
    cacheNode_customerOutpostArn,
    cacheNode_endpoint,
    cacheNode_parameterGroupStatus,
    cacheNode_sourceCacheNodeId,

    -- * CacheNodeTypeSpecificParameter
    CacheNodeTypeSpecificParameter (..),
    newCacheNodeTypeSpecificParameter,
    cacheNodeTypeSpecificParameter_allowedValues,
    cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues,
    cacheNodeTypeSpecificParameter_changeType,
    cacheNodeTypeSpecificParameter_dataType,
    cacheNodeTypeSpecificParameter_description,
    cacheNodeTypeSpecificParameter_isModifiable,
    cacheNodeTypeSpecificParameter_minimumEngineVersion,
    cacheNodeTypeSpecificParameter_parameterName,
    cacheNodeTypeSpecificParameter_source,

    -- * CacheNodeTypeSpecificValue
    CacheNodeTypeSpecificValue (..),
    newCacheNodeTypeSpecificValue,
    cacheNodeTypeSpecificValue_cacheNodeType,
    cacheNodeTypeSpecificValue_value,

    -- * CacheNodeUpdateStatus
    CacheNodeUpdateStatus (..),
    newCacheNodeUpdateStatus,
    cacheNodeUpdateStatus_cacheNodeId,
    cacheNodeUpdateStatus_nodeDeletionDate,
    cacheNodeUpdateStatus_nodeUpdateEndDate,
    cacheNodeUpdateStatus_nodeUpdateInitiatedBy,
    cacheNodeUpdateStatus_nodeUpdateInitiatedDate,
    cacheNodeUpdateStatus_nodeUpdateStartDate,
    cacheNodeUpdateStatus_nodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate,

    -- * CacheParameterGroup
    CacheParameterGroup (..),
    newCacheParameterGroup,
    cacheParameterGroup_arn,
    cacheParameterGroup_cacheParameterGroupFamily,
    cacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroup_description,
    cacheParameterGroup_isGlobal,

    -- * CacheParameterGroupNameMessage
    CacheParameterGroupNameMessage (..),
    newCacheParameterGroupNameMessage,
    cacheParameterGroupNameMessage_cacheParameterGroupName,

    -- * CacheParameterGroupStatus
    CacheParameterGroupStatus (..),
    newCacheParameterGroupStatus,
    cacheParameterGroupStatus_cacheNodeIdsToReboot,
    cacheParameterGroupStatus_cacheParameterGroupName,
    cacheParameterGroupStatus_parameterApplyStatus,

    -- * CacheSecurityGroup
    CacheSecurityGroup (..),
    newCacheSecurityGroup,
    cacheSecurityGroup_arn,
    cacheSecurityGroup_cacheSecurityGroupName,
    cacheSecurityGroup_description,
    cacheSecurityGroup_eC2SecurityGroups,
    cacheSecurityGroup_ownerId,

    -- * CacheSecurityGroupMembership
    CacheSecurityGroupMembership (..),
    newCacheSecurityGroupMembership,
    cacheSecurityGroupMembership_cacheSecurityGroupName,
    cacheSecurityGroupMembership_status,

    -- * CacheSubnetGroup
    CacheSubnetGroup (..),
    newCacheSubnetGroup,
    cacheSubnetGroup_arn,
    cacheSubnetGroup_cacheSubnetGroupDescription,
    cacheSubnetGroup_cacheSubnetGroupName,
    cacheSubnetGroup_subnets,
    cacheSubnetGroup_supportedNetworkTypes,
    cacheSubnetGroup_vpcId,

    -- * CloudWatchLogsDestinationDetails
    CloudWatchLogsDestinationDetails (..),
    newCloudWatchLogsDestinationDetails,
    cloudWatchLogsDestinationDetails_logGroup,

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

    -- * DestinationDetails
    DestinationDetails (..),
    newDestinationDetails,
    destinationDetails_cloudWatchLogsDetails,
    destinationDetails_kinesisFirehoseDetails,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_eC2SecurityGroupName,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,

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
    engineDefaults_marker,
    engineDefaults_parameters,

    -- * Event
    Event (..),
    newEvent,
    event_date,
    event_message,
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
    globalReplicationGroup_arn,
    globalReplicationGroup_atRestEncryptionEnabled,
    globalReplicationGroup_authTokenEnabled,
    globalReplicationGroup_cacheNodeType,
    globalReplicationGroup_clusterEnabled,
    globalReplicationGroup_engine,
    globalReplicationGroup_engineVersion,
    globalReplicationGroup_globalNodeGroups,
    globalReplicationGroup_globalReplicationGroupDescription,
    globalReplicationGroup_globalReplicationGroupId,
    globalReplicationGroup_members,
    globalReplicationGroup_status,
    globalReplicationGroup_transitEncryptionEnabled,

    -- * GlobalReplicationGroupInfo
    GlobalReplicationGroupInfo (..),
    newGlobalReplicationGroupInfo,
    globalReplicationGroupInfo_globalReplicationGroupId,
    globalReplicationGroupInfo_globalReplicationGroupMemberRole,

    -- * GlobalReplicationGroupMember
    GlobalReplicationGroupMember (..),
    newGlobalReplicationGroupMember,
    globalReplicationGroupMember_automaticFailover,
    globalReplicationGroupMember_replicationGroupId,
    globalReplicationGroupMember_replicationGroupRegion,
    globalReplicationGroupMember_role,
    globalReplicationGroupMember_status,

    -- * KinesisFirehoseDestinationDetails
    KinesisFirehoseDestinationDetails (..),
    newKinesisFirehoseDestinationDetails,
    kinesisFirehoseDestinationDetails_deliveryStream,

    -- * LogDeliveryConfiguration
    LogDeliveryConfiguration (..),
    newLogDeliveryConfiguration,
    logDeliveryConfiguration_destinationDetails,
    logDeliveryConfiguration_destinationType,
    logDeliveryConfiguration_logFormat,
    logDeliveryConfiguration_logType,
    logDeliveryConfiguration_message,
    logDeliveryConfiguration_status,

    -- * LogDeliveryConfigurationRequest
    LogDeliveryConfigurationRequest (..),
    newLogDeliveryConfigurationRequest,
    logDeliveryConfigurationRequest_destinationDetails,
    logDeliveryConfigurationRequest_destinationType,
    logDeliveryConfigurationRequest_enabled,
    logDeliveryConfigurationRequest_logFormat,
    logDeliveryConfigurationRequest_logType,

    -- * NodeGroup
    NodeGroup (..),
    newNodeGroup,
    nodeGroup_nodeGroupId,
    nodeGroup_nodeGroupMembers,
    nodeGroup_primaryEndpoint,
    nodeGroup_readerEndpoint,
    nodeGroup_slots,
    nodeGroup_status,

    -- * NodeGroupConfiguration
    NodeGroupConfiguration (..),
    newNodeGroupConfiguration,
    nodeGroupConfiguration_nodeGroupId,
    nodeGroupConfiguration_primaryAvailabilityZone,
    nodeGroupConfiguration_primaryOutpostArn,
    nodeGroupConfiguration_replicaAvailabilityZones,
    nodeGroupConfiguration_replicaCount,
    nodeGroupConfiguration_replicaOutpostArns,
    nodeGroupConfiguration_slots,

    -- * NodeGroupMember
    NodeGroupMember (..),
    newNodeGroupMember,
    nodeGroupMember_cacheClusterId,
    nodeGroupMember_cacheNodeId,
    nodeGroupMember_currentRole,
    nodeGroupMember_preferredAvailabilityZone,
    nodeGroupMember_preferredOutpostArn,
    nodeGroupMember_readEndpoint,

    -- * NodeGroupMemberUpdateStatus
    NodeGroupMemberUpdateStatus (..),
    newNodeGroupMemberUpdateStatus,
    nodeGroupMemberUpdateStatus_cacheClusterId,
    nodeGroupMemberUpdateStatus_cacheNodeId,
    nodeGroupMemberUpdateStatus_nodeDeletionDate,
    nodeGroupMemberUpdateStatus_nodeUpdateEndDate,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStartDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStatus,
    nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate,

    -- * NodeGroupUpdateStatus
    NodeGroupUpdateStatus (..),
    newNodeGroupUpdateStatus,
    nodeGroupUpdateStatus_nodeGroupId,
    nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus,

    -- * NodeSnapshot
    NodeSnapshot (..),
    newNodeSnapshot,
    nodeSnapshot_cacheClusterId,
    nodeSnapshot_cacheNodeCreateTime,
    nodeSnapshot_cacheNodeId,
    nodeSnapshot_cacheSize,
    nodeSnapshot_nodeGroupConfiguration,
    nodeSnapshot_nodeGroupId,
    nodeSnapshot_snapshotCreateTime,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_topicArn,
    notificationConfiguration_topicStatus,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_allowedValues,
    parameter_changeType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_minimumEngineVersion,
    parameter_parameterName,
    parameter_parameterValue,
    parameter_source,

    -- * ParameterNameValue
    ParameterNameValue (..),
    newParameterNameValue,
    parameterNameValue_parameterName,
    parameterNameValue_parameterValue,

    -- * PendingLogDeliveryConfiguration
    PendingLogDeliveryConfiguration (..),
    newPendingLogDeliveryConfiguration,
    pendingLogDeliveryConfiguration_destinationDetails,
    pendingLogDeliveryConfiguration_destinationType,
    pendingLogDeliveryConfiguration_logFormat,
    pendingLogDeliveryConfiguration_logType,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
    pendingModifiedValues_authTokenStatus,
    pendingModifiedValues_cacheNodeIdsToRemove,
    pendingModifiedValues_cacheNodeType,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_logDeliveryConfigurations,
    pendingModifiedValues_numCacheNodes,
    pendingModifiedValues_transitEncryptionEnabled,
    pendingModifiedValues_transitEncryptionMode,

    -- * ProcessedUpdateAction
    ProcessedUpdateAction (..),
    newProcessedUpdateAction,
    processedUpdateAction_cacheClusterId,
    processedUpdateAction_replicationGroupId,
    processedUpdateAction_serviceUpdateName,
    processedUpdateAction_updateActionStatus,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeAmount,
    recurringCharge_recurringChargeFrequency,

    -- * RegionalConfiguration
    RegionalConfiguration (..),
    newRegionalConfiguration,
    regionalConfiguration_replicationGroupId,
    regionalConfiguration_replicationGroupRegion,
    regionalConfiguration_reshardingConfiguration,

    -- * ReplicationGroup
    ReplicationGroup (..),
    newReplicationGroup,
    replicationGroup_arn,
    replicationGroup_atRestEncryptionEnabled,
    replicationGroup_authTokenEnabled,
    replicationGroup_authTokenLastModifiedDate,
    replicationGroup_autoMinorVersionUpgrade,
    replicationGroup_automaticFailover,
    replicationGroup_cacheNodeType,
    replicationGroup_clusterEnabled,
    replicationGroup_clusterMode,
    replicationGroup_configurationEndpoint,
    replicationGroup_dataTiering,
    replicationGroup_description,
    replicationGroup_globalReplicationGroupInfo,
    replicationGroup_ipDiscovery,
    replicationGroup_kmsKeyId,
    replicationGroup_logDeliveryConfigurations,
    replicationGroup_memberClusters,
    replicationGroup_memberClustersOutpostArns,
    replicationGroup_multiAZ,
    replicationGroup_networkType,
    replicationGroup_nodeGroups,
    replicationGroup_pendingModifiedValues,
    replicationGroup_replicationGroupCreateTime,
    replicationGroup_replicationGroupId,
    replicationGroup_snapshotRetentionLimit,
    replicationGroup_snapshotWindow,
    replicationGroup_snapshottingClusterId,
    replicationGroup_status,
    replicationGroup_transitEncryptionEnabled,
    replicationGroup_transitEncryptionMode,
    replicationGroup_userGroupIds,

    -- * ReplicationGroupPendingModifiedValues
    ReplicationGroupPendingModifiedValues (..),
    newReplicationGroupPendingModifiedValues,
    replicationGroupPendingModifiedValues_authTokenStatus,
    replicationGroupPendingModifiedValues_automaticFailoverStatus,
    replicationGroupPendingModifiedValues_clusterMode,
    replicationGroupPendingModifiedValues_logDeliveryConfigurations,
    replicationGroupPendingModifiedValues_primaryClusterId,
    replicationGroupPendingModifiedValues_resharding,
    replicationGroupPendingModifiedValues_transitEncryptionEnabled,
    replicationGroupPendingModifiedValues_transitEncryptionMode,
    replicationGroupPendingModifiedValues_userGroups,

    -- * ReservedCacheNode
    ReservedCacheNode (..),
    newReservedCacheNode,
    reservedCacheNode_cacheNodeCount,
    reservedCacheNode_cacheNodeType,
    reservedCacheNode_duration,
    reservedCacheNode_fixedPrice,
    reservedCacheNode_offeringType,
    reservedCacheNode_productDescription,
    reservedCacheNode_recurringCharges,
    reservedCacheNode_reservationARN,
    reservedCacheNode_reservedCacheNodeId,
    reservedCacheNode_reservedCacheNodesOfferingId,
    reservedCacheNode_startTime,
    reservedCacheNode_state,
    reservedCacheNode_usagePrice,

    -- * ReservedCacheNodesOffering
    ReservedCacheNodesOffering (..),
    newReservedCacheNodesOffering,
    reservedCacheNodesOffering_cacheNodeType,
    reservedCacheNodesOffering_duration,
    reservedCacheNodesOffering_fixedPrice,
    reservedCacheNodesOffering_offeringType,
    reservedCacheNodesOffering_productDescription,
    reservedCacheNodesOffering_recurringCharges,
    reservedCacheNodesOffering_reservedCacheNodesOfferingId,
    reservedCacheNodesOffering_usagePrice,

    -- * ReshardingConfiguration
    ReshardingConfiguration (..),
    newReshardingConfiguration,
    reshardingConfiguration_nodeGroupId,
    reshardingConfiguration_preferredAvailabilityZones,

    -- * ReshardingStatus
    ReshardingStatus (..),
    newReshardingStatus,
    reshardingStatus_slotMigration,

    -- * SecurityGroupMembership
    SecurityGroupMembership (..),
    newSecurityGroupMembership,
    securityGroupMembership_securityGroupId,
    securityGroupMembership_status,

    -- * ServiceUpdate
    ServiceUpdate (..),
    newServiceUpdate,
    serviceUpdate_autoUpdateAfterRecommendedApplyByDate,
    serviceUpdate_engine,
    serviceUpdate_engineVersion,
    serviceUpdate_estimatedUpdateTime,
    serviceUpdate_serviceUpdateDescription,
    serviceUpdate_serviceUpdateEndDate,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_serviceUpdateRecommendedApplyByDate,
    serviceUpdate_serviceUpdateReleaseDate,
    serviceUpdate_serviceUpdateSeverity,
    serviceUpdate_serviceUpdateStatus,
    serviceUpdate_serviceUpdateType,

    -- * SlotMigration
    SlotMigration (..),
    newSlotMigration,
    slotMigration_progressPercentage,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_arn,
    snapshot_autoMinorVersionUpgrade,
    snapshot_automaticFailover,
    snapshot_cacheClusterCreateTime,
    snapshot_cacheClusterId,
    snapshot_cacheNodeType,
    snapshot_cacheParameterGroupName,
    snapshot_cacheSubnetGroupName,
    snapshot_dataTiering,
    snapshot_engine,
    snapshot_engineVersion,
    snapshot_kmsKeyId,
    snapshot_nodeSnapshots,
    snapshot_numCacheNodes,
    snapshot_numNodeGroups,
    snapshot_port,
    snapshot_preferredAvailabilityZone,
    snapshot_preferredMaintenanceWindow,
    snapshot_preferredOutpostArn,
    snapshot_replicationGroupDescription,
    snapshot_replicationGroupId,
    snapshot_snapshotName,
    snapshot_snapshotRetentionLimit,
    snapshot_snapshotSource,
    snapshot_snapshotStatus,
    snapshot_snapshotWindow,
    snapshot_topicArn,
    snapshot_vpcId,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetOutpost,
    subnet_supportedNetworkTypes,

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
    timeRangeFilter_endTime,
    timeRangeFilter_startTime,

    -- * UnprocessedUpdateAction
    UnprocessedUpdateAction (..),
    newUnprocessedUpdateAction,
    unprocessedUpdateAction_cacheClusterId,
    unprocessedUpdateAction_errorMessage,
    unprocessedUpdateAction_errorType,
    unprocessedUpdateAction_replicationGroupId,
    unprocessedUpdateAction_serviceUpdateName,

    -- * UpdateAction
    UpdateAction (..),
    newUpdateAction,
    updateAction_cacheClusterId,
    updateAction_cacheNodeUpdateStatus,
    updateAction_engine,
    updateAction_estimatedUpdateTime,
    updateAction_nodeGroupUpdateStatus,
    updateAction_nodesUpdated,
    updateAction_replicationGroupId,
    updateAction_serviceUpdateName,
    updateAction_serviceUpdateRecommendedApplyByDate,
    updateAction_serviceUpdateReleaseDate,
    updateAction_serviceUpdateSeverity,
    updateAction_serviceUpdateStatus,
    updateAction_serviceUpdateType,
    updateAction_slaMet,
    updateAction_updateActionAvailableDate,
    updateAction_updateActionStatus,
    updateAction_updateActionStatusModifiedDate,

    -- * UpdateActionResultsMessage
    UpdateActionResultsMessage (..),
    newUpdateActionResultsMessage,
    updateActionResultsMessage_processedUpdateActions,
    updateActionResultsMessage_unprocessedUpdateActions,

    -- * User
    User (..),
    newUser,
    user_arn,
    user_accessString,
    user_authentication,
    user_engine,
    user_minimumEngineVersion,
    user_status,
    user_userGroupIds,
    user_userId,
    user_userName,

    -- * UserGroup
    UserGroup (..),
    newUserGroup,
    userGroup_arn,
    userGroup_engine,
    userGroup_minimumEngineVersion,
    userGroup_pendingChanges,
    userGroup_replicationGroups,
    userGroup_status,
    userGroup_userGroupId,
    userGroup_userIds,

    -- * UserGroupPendingChanges
    UserGroupPendingChanges (..),
    newUserGroupPendingChanges,
    userGroupPendingChanges_userIdsToAdd,
    userGroupPendingChanges_userIdsToRemove,

    -- * UserGroupsUpdateStatus
    UserGroupsUpdateStatus (..),
    newUserGroupsUpdateStatus,
    userGroupsUpdateStatus_userGroupIdsToAdd,
    userGroupsUpdateStatus_userGroupIdsToRemove,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElastiCache.Types.AZMode
import Amazonka.ElastiCache.Types.AuthTokenUpdateStatus
import Amazonka.ElastiCache.Types.AuthTokenUpdateStrategyType
import Amazonka.ElastiCache.Types.Authentication
import Amazonka.ElastiCache.Types.AuthenticationMode
import Amazonka.ElastiCache.Types.AuthenticationType
import Amazonka.ElastiCache.Types.AutomaticFailoverStatus
import Amazonka.ElastiCache.Types.AvailabilityZone
import Amazonka.ElastiCache.Types.CacheCluster
import Amazonka.ElastiCache.Types.CacheEngineVersion
import Amazonka.ElastiCache.Types.CacheNode
import Amazonka.ElastiCache.Types.CacheNodeTypeSpecificParameter
import Amazonka.ElastiCache.Types.CacheNodeTypeSpecificValue
import Amazonka.ElastiCache.Types.CacheNodeUpdateStatus
import Amazonka.ElastiCache.Types.CacheParameterGroup
import Amazonka.ElastiCache.Types.CacheParameterGroupNameMessage
import Amazonka.ElastiCache.Types.CacheParameterGroupStatus
import Amazonka.ElastiCache.Types.CacheSecurityGroup
import Amazonka.ElastiCache.Types.CacheSecurityGroupMembership
import Amazonka.ElastiCache.Types.CacheSubnetGroup
import Amazonka.ElastiCache.Types.ChangeType
import Amazonka.ElastiCache.Types.CloudWatchLogsDestinationDetails
import Amazonka.ElastiCache.Types.ClusterMode
import Amazonka.ElastiCache.Types.ConfigureShard
import Amazonka.ElastiCache.Types.CustomerNodeEndpoint
import Amazonka.ElastiCache.Types.DataTieringStatus
import Amazonka.ElastiCache.Types.DestinationDetails
import Amazonka.ElastiCache.Types.DestinationType
import Amazonka.ElastiCache.Types.EC2SecurityGroup
import Amazonka.ElastiCache.Types.Endpoint
import Amazonka.ElastiCache.Types.EngineDefaults
import Amazonka.ElastiCache.Types.Event
import Amazonka.ElastiCache.Types.Filter
import Amazonka.ElastiCache.Types.GlobalNodeGroup
import Amazonka.ElastiCache.Types.GlobalReplicationGroup
import Amazonka.ElastiCache.Types.GlobalReplicationGroupInfo
import Amazonka.ElastiCache.Types.GlobalReplicationGroupMember
import Amazonka.ElastiCache.Types.InputAuthenticationType
import Amazonka.ElastiCache.Types.IpDiscovery
import Amazonka.ElastiCache.Types.KinesisFirehoseDestinationDetails
import Amazonka.ElastiCache.Types.LogDeliveryConfiguration
import Amazonka.ElastiCache.Types.LogDeliveryConfigurationRequest
import Amazonka.ElastiCache.Types.LogDeliveryConfigurationStatus
import Amazonka.ElastiCache.Types.LogFormat
import Amazonka.ElastiCache.Types.LogType
import Amazonka.ElastiCache.Types.MultiAZStatus
import Amazonka.ElastiCache.Types.NetworkType
import Amazonka.ElastiCache.Types.NodeGroup
import Amazonka.ElastiCache.Types.NodeGroupConfiguration
import Amazonka.ElastiCache.Types.NodeGroupMember
import Amazonka.ElastiCache.Types.NodeGroupMemberUpdateStatus
import Amazonka.ElastiCache.Types.NodeGroupUpdateStatus
import Amazonka.ElastiCache.Types.NodeSnapshot
import Amazonka.ElastiCache.Types.NodeUpdateInitiatedBy
import Amazonka.ElastiCache.Types.NodeUpdateStatus
import Amazonka.ElastiCache.Types.NotificationConfiguration
import Amazonka.ElastiCache.Types.OutpostMode
import Amazonka.ElastiCache.Types.Parameter
import Amazonka.ElastiCache.Types.ParameterNameValue
import Amazonka.ElastiCache.Types.PendingAutomaticFailoverStatus
import Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration
import Amazonka.ElastiCache.Types.PendingModifiedValues
import Amazonka.ElastiCache.Types.ProcessedUpdateAction
import Amazonka.ElastiCache.Types.RecurringCharge
import Amazonka.ElastiCache.Types.RegionalConfiguration
import Amazonka.ElastiCache.Types.ReplicationGroup
import Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues
import Amazonka.ElastiCache.Types.ReservedCacheNode
import Amazonka.ElastiCache.Types.ReservedCacheNodesOffering
import Amazonka.ElastiCache.Types.ReshardingConfiguration
import Amazonka.ElastiCache.Types.ReshardingStatus
import Amazonka.ElastiCache.Types.SecurityGroupMembership
import Amazonka.ElastiCache.Types.ServiceUpdate
import Amazonka.ElastiCache.Types.ServiceUpdateSeverity
import Amazonka.ElastiCache.Types.ServiceUpdateStatus
import Amazonka.ElastiCache.Types.ServiceUpdateType
import Amazonka.ElastiCache.Types.SlaMet
import Amazonka.ElastiCache.Types.SlotMigration
import Amazonka.ElastiCache.Types.Snapshot
import Amazonka.ElastiCache.Types.SourceType
import Amazonka.ElastiCache.Types.Subnet
import Amazonka.ElastiCache.Types.SubnetOutpost
import Amazonka.ElastiCache.Types.Tag
import Amazonka.ElastiCache.Types.TagListMessage
import Amazonka.ElastiCache.Types.TimeRangeFilter
import Amazonka.ElastiCache.Types.TransitEncryptionMode
import Amazonka.ElastiCache.Types.UnprocessedUpdateAction
import Amazonka.ElastiCache.Types.UpdateAction
import Amazonka.ElastiCache.Types.UpdateActionResultsMessage
import Amazonka.ElastiCache.Types.UpdateActionStatus
import Amazonka.ElastiCache.Types.User
import Amazonka.ElastiCache.Types.UserGroup
import Amazonka.ElastiCache.Types.UserGroupPendingChanges
import Amazonka.ElastiCache.Types.UserGroupsUpdateStatus
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-02-02@ of the Amazon ElastiCache SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "ElastiCache",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "elasticache",
      Core.signingName = "elasticache",
      Core.version = "2015-02-02",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "ElastiCache",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | The customer has exceeded the allowed rate of API calls.
_APICallRateForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_APICallRateForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "APICallRateForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon EC2 security group is already authorized for the
-- specified cache security group.
_AuthorizationAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon EC2 security group is not authorized for the
-- specified cache security group.
_AuthorizationNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | You already have a cluster with the given identifier.
_CacheClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheClusterAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested cluster ID does not refer to an existing cluster.
_CacheClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheClusterNotFound"
    Prelude.. Core.hasStatus 404

-- | A cache parameter group with the requested name already exists.
_CacheParameterGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested cache parameter group name does not refer to an existing
-- cache parameter group.
_CacheParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the maximum
-- number of cache security groups.
_CacheParameterGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | A cache security group with the specified name already exists.
_CacheSecurityGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested cache security group name does not refer to an existing
-- cache security group.
_CacheSecurityGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the allowed
-- number of cache security groups.
_CacheSecurityGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.CacheSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The requested cache subnet group name is already in use by an existing
-- cache subnet group.
_CacheSubnetGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested cache subnet group is currently in use.
_CacheSubnetGroupInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSubnetGroupInUse =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupInUse"
    Prelude.. Core.hasStatus 400

-- | The requested cache subnet group name does not refer to an existing
-- cache subnet group.
_CacheSubnetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache subnet groups.
_CacheSubnetGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a cache subnet group.
_CacheSubnetQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CacheSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of clusters per customer.
_ClusterQuotaForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The default user assigned to the user group.
_DefaultUserAssociatedToUserGroupFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DefaultUserAssociatedToUserGroupFault =
  Core._MatchServiceError
    defaultService
    "DefaultUserAssociatedToUserGroup"
    Prelude.. Core.hasStatus 400

-- | You must add default user to a user group.
_DefaultUserRequired :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DefaultUserRequired =
  Core._MatchServiceError
    defaultService
    "DefaultUserRequired"
    Prelude.. Core.hasStatus 400

-- | A user with this username already exists.
_DuplicateUserNameFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DuplicateUserNameFault =
  Core._MatchServiceError
    defaultService
    "DuplicateUserName"
    Prelude.. Core.hasStatus 400

-- | The Global datastore name already exists.
_GlobalReplicationGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalReplicationGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalReplicationGroupAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The Global datastore does not exist
_GlobalReplicationGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalReplicationGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalReplicationGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The requested cache node type is not available in the specified
-- Availability Zone. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/ErrorMessages.html#ErrorMessages.INSUFFICIENT_CACHE_CLUSTER_CAPACITY InsufficientCacheClusterCapacity>
-- in the ElastiCache User Guide.
_InsufficientCacheClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientCacheClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientCacheClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidARNFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError defaultService "InvalidARN"
    Prelude.. Core.hasStatus 400

-- | The requested cluster is not in the @available@ state.
_InvalidCacheClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCacheClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheClusterState"
    Prelude.. Core.hasStatus 400

-- | The current state of the cache parameter group does not allow the
-- requested operation to occur.
_InvalidCacheParameterGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCacheParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The current state of the cache security group does not allow deletion.
_InvalidCacheSecurityGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidCacheSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The Global datastore is not available or in primary-only state.
_InvalidGlobalReplicationGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGlobalReplicationGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalReplicationGroupState"
    Prelude.. Core.hasStatus 400

-- | The KMS key supplied is not valid.
_InvalidKMSKeyFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidKMSKeyFault =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyFault"
    Prelude.. Core.hasStatus 400

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombination"
    Prelude.. Core.hasStatus 400

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValue"
    Prelude.. Core.hasStatus 400

-- | The requested replication group is not in the @available@ state.
_InvalidReplicationGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidReplicationGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidReplicationGroupState"
    Prelude.. Core.hasStatus 400

-- | The current state of the snapshot does not allow the requested operation
-- to occur.
_InvalidSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotState"
    Prelude.. Core.hasStatus 400

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The user group is not in an active state.
_InvalidUserGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidUserGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidUserGroupState"
    Prelude.. Core.hasStatus 400

-- | The user is not in active state.
_InvalidUserStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidUserStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidUserState"
    Prelude.. Core.hasStatus 400

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The operation was not performed because no changes were required.
_NoOperationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NoOperationFault =
  Core._MatchServiceError
    defaultService
    "NoOperationFault"
    Prelude.. Core.hasStatus 400

-- | The node group specified by the @NodeGroupId@ parameter could not be
-- found. Please verify that the node group exists and that you spelled the
-- @NodeGroupId@ value correctly.
_NodeGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "NodeGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the maximum
-- allowed number of node groups (shards) in a single replication group.
-- The default maximum is 90
_NodeGroupsPerReplicationGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeGroupsPerReplicationGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeGroupsPerReplicationGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes in a single cluster.
_NodeQuotaForClusterExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceeded"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes per customer.
_NodeQuotaForCustomerExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The specified replication group already exists.
_ReplicationGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReplicationGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The targeted replication group is not available.
_ReplicationGroupAlreadyUnderMigrationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReplicationGroupAlreadyUnderMigrationFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupAlreadyUnderMigrationFault"
    Prelude.. Core.hasStatus 400

-- | The specified replication group does not exist.
_ReplicationGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReplicationGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The designated replication group is not available for data migration.
_ReplicationGroupNotUnderMigrationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReplicationGroupNotUnderMigrationFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupNotUnderMigrationFault"
    Prelude.. Core.hasStatus 400

-- | You already have a reservation with the given identifier.
_ReservedCacheNodeAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedCacheNodeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | The requested reserved cache node was not found.
_ReservedCacheNodeNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedCacheNodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeNotFound"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the user\'s
-- cache node quota.
_ReservedCacheNodeQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedCacheNodeQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested cache node offering does not exist.
_ReservedCacheNodesOfferingNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ReservedCacheNodesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodesOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The service update doesn\'t exist
_ServiceUpdateNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceUpdateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceUpdateNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You already have a snapshot with the given name.
_SnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotAlreadyExistsFault"
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
_SnapshotFeatureNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotFeatureNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "SnapshotFeatureNotSupportedFault"
    Prelude.. Core.hasStatus 400

-- | The requested snapshot name does not refer to an existing snapshot.
_SnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the maximum
-- number of snapshots.
_SnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is being used by another cache subnet group.
_SubnetInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError
    defaultService
    "SubnetInUse"
    Prelude.. Core.hasStatus 400

-- | At least one subnet ID does not match the other subnet IDs. This
-- mismatch typically occurs when a user sets one subnet ID to a regional
-- Availability Zone and a different one to an outpost. Or when a user sets
-- the subnet ID to an Outpost when not subscribed on this service.
_SubnetNotAllowedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetNotAllowedFault =
  Core._MatchServiceError
    defaultService
    "SubnetNotAllowedFault"
    Prelude.. Core.hasStatus 400

-- | The requested tag was not found on this resource.
_TagNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFound"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would cause the resource to
-- have more than the allowed number of tags. The maximum number of tags
-- permitted on a resource is 50.
_TagQuotaPerResourceExceeded :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TagQuotaPerResourceExceeded =
  Core._MatchServiceError
    defaultService
    "TagQuotaPerResourceExceeded"
    Prelude.. Core.hasStatus 400

-- | The @TestFailover@ action is not available.
_TestFailoverNotAvailableFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TestFailoverNotAvailableFault =
  Core._MatchServiceError
    defaultService
    "TestFailoverNotAvailableFault"
    Prelude.. Core.hasStatus 400

-- | A user with this ID already exists.
_UserAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The user group with this ID already exists.
_UserGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The user group was not found or does not exist
_UserGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The number of users exceeds the user group limit.
_UserGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The user does not exist or could not be found.
_UserNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserNotFound"
    Prelude.. Core.hasStatus 404

-- | The quota of users has been exceeded.
_UserQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UserQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserQuotaExceeded"
    Prelude.. Core.hasStatus 400
