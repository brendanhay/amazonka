{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElastiCache.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _CacheParameterGroupAlreadyExistsFault,
    _CacheSecurityGroupAlreadyExistsFault,
    _NodeQuotaForClusterExceededFault,
    _ClusterQuotaForCustomerExceededFault,
    _SnapshotQuotaExceededFault,
    _UserGroupNotFoundFault,
    _InvalidSubnet,
    _ReplicationGroupNotFoundFault,
    _NodeGroupsPerReplicationGroupQuotaExceededFault,
    _AuthorizationAlreadyExistsFault,
    _CacheSubnetGroupNotFoundFault,
    _ServiceUpdateNotFoundFault,
    _UserNotFoundFault,
    _InvalidSnapshotStateFault,
    _CacheClusterNotFoundFault,
    _CacheSubnetGroupQuotaExceededFault,
    _ReservedCacheNodeAlreadyExistsFault,
    _NoOperationFault,
    _UserAlreadyExistsFault,
    _TagNotFoundFault,
    _InvalidGlobalReplicationGroupStateFault,
    _DefaultUserRequired,
    _InvalidParameterCombinationException,
    _ServiceLinkedRoleNotFoundFault,
    _AuthorizationNotFoundFault,
    _UserQuotaExceededFault,
    _GlobalReplicationGroupNotFoundFault,
    _ReplicationGroupAlreadyExistsFault,
    _DuplicateUserNameFault,
    _CacheParameterGroupQuotaExceededFault,
    _CacheSecurityGroupQuotaExceededFault,
    _ReplicationGroupNotUnderMigrationFault,
    _DefaultUserAssociatedToUserGroupFault,
    _TagQuotaPerResourceExceeded,
    _CacheParameterGroupNotFoundFault,
    _InvalidARNFault,
    _GlobalReplicationGroupAlreadyExistsFault,
    _CacheSecurityGroupNotFoundFault,
    _SnapshotAlreadyExistsFault,
    _TestFailoverNotAvailableFault,
    _ReservedCacheNodesOfferingNotFoundFault,
    _CacheSubnetGroupInUse,
    _NodeQuotaForCustomerExceededFault,
    _InvalidUserGroupStateFault,
    _NodeGroupNotFoundFault,
    _InvalidCacheSecurityGroupStateFault,
    _InvalidVPCNetworkStateFault,
    _SubnetInUse,
    _InsufficientCacheClusterCapacityFault,
    _InvalidUserStateFault,
    _SnapshotNotFoundFault,
    _ReservedCacheNodeQuotaExceededFault,
    _CacheSubnetGroupAlreadyExistsFault,
    _UserGroupAlreadyExistsFault,
    _CacheSubnetQuotaExceededFault,
    _InvalidKMSKeyFault,
    _CacheClusterAlreadyExistsFault,
    _ReplicationGroupAlreadyUnderMigrationFault,
    _SnapshotFeatureNotSupportedFault,
    _InvalidCacheClusterStateFault,
    _InvalidCacheParameterGroupStateFault,
    _InvalidParameterValueException,
    _SubnetNotAllowedFault,
    _APICallRateForCustomerExceededFault,
    _ReservedCacheNodeNotFoundFault,
    _InvalidReplicationGroupStateFault,
    _UserGroupQuotaExceededFault,

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

    -- * UpdateActionStatus
    UpdateActionStatus (..),

    -- * Authentication
    Authentication (..),
    newAuthentication,
    authentication_type,
    authentication_passwordCount,

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
    cacheCluster_transitEncryptionEnabled,
    cacheCluster_cacheSubnetGroupName,
    cacheCluster_replicationGroupLogDeliveryEnabled,
    cacheCluster_autoMinorVersionUpgrade,
    cacheCluster_cacheClusterStatus,
    cacheCluster_clientDownloadLandingPage,
    cacheCluster_arn,
    cacheCluster_logDeliveryConfigurations,
    cacheCluster_cacheClusterCreateTime,
    cacheCluster_atRestEncryptionEnabled,
    cacheCluster_ipDiscovery,
    cacheCluster_numCacheNodes,
    cacheCluster_notificationConfiguration,
    cacheCluster_cacheNodeType,
    cacheCluster_preferredAvailabilityZone,
    cacheCluster_cacheNodes,
    cacheCluster_cacheClusterId,
    cacheCluster_authTokenEnabled,
    cacheCluster_snapshotWindow,
    cacheCluster_snapshotRetentionLimit,
    cacheCluster_cacheParameterGroup,
    cacheCluster_securityGroups,
    cacheCluster_preferredOutpostArn,
    cacheCluster_engine,
    cacheCluster_pendingModifiedValues,
    cacheCluster_preferredMaintenanceWindow,
    cacheCluster_authTokenLastModifiedDate,
    cacheCluster_replicationGroupId,
    cacheCluster_engineVersion,
    cacheCluster_networkType,
    cacheCluster_cacheSecurityGroups,
    cacheCluster_configurationEndpoint,

    -- * CacheEngineVersion
    CacheEngineVersion (..),
    newCacheEngineVersion,
    cacheEngineVersion_cacheEngineVersionDescription,
    cacheEngineVersion_engine,
    cacheEngineVersion_cacheParameterGroupFamily,
    cacheEngineVersion_cacheEngineDescription,
    cacheEngineVersion_engineVersion,

    -- * CacheNode
    CacheNode (..),
    newCacheNode,
    cacheNode_customerOutpostArn,
    cacheNode_customerAvailabilityZone,
    cacheNode_sourceCacheNodeId,
    cacheNode_cacheNodeId,
    cacheNode_cacheNodeStatus,
    cacheNode_parameterGroupStatus,
    cacheNode_endpoint,
    cacheNode_cacheNodeCreateTime,

    -- * CacheNodeTypeSpecificParameter
    CacheNodeTypeSpecificParameter (..),
    newCacheNodeTypeSpecificParameter,
    cacheNodeTypeSpecificParameter_changeType,
    cacheNodeTypeSpecificParameter_isModifiable,
    cacheNodeTypeSpecificParameter_description,
    cacheNodeTypeSpecificParameter_parameterName,
    cacheNodeTypeSpecificParameter_cacheNodeTypeSpecificValues,
    cacheNodeTypeSpecificParameter_minimumEngineVersion,
    cacheNodeTypeSpecificParameter_source,
    cacheNodeTypeSpecificParameter_allowedValues,
    cacheNodeTypeSpecificParameter_dataType,

    -- * CacheNodeTypeSpecificValue
    CacheNodeTypeSpecificValue (..),
    newCacheNodeTypeSpecificValue,
    cacheNodeTypeSpecificValue_cacheNodeType,
    cacheNodeTypeSpecificValue_value,

    -- * CacheNodeUpdateStatus
    CacheNodeUpdateStatus (..),
    newCacheNodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateStatus,
    cacheNodeUpdateStatus_nodeUpdateEndDate,
    cacheNodeUpdateStatus_nodeUpdateStartDate,
    cacheNodeUpdateStatus_nodeDeletionDate,
    cacheNodeUpdateStatus_nodeUpdateInitiatedBy,
    cacheNodeUpdateStatus_nodeUpdateInitiatedDate,
    cacheNodeUpdateStatus_nodeUpdateStatusModifiedDate,
    cacheNodeUpdateStatus_cacheNodeId,

    -- * CacheParameterGroup
    CacheParameterGroup (..),
    newCacheParameterGroup,
    cacheParameterGroup_isGlobal,
    cacheParameterGroup_arn,
    cacheParameterGroup_description,
    cacheParameterGroup_cacheParameterGroupName,
    cacheParameterGroup_cacheParameterGroupFamily,

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
    cacheSecurityGroup_description,
    cacheSecurityGroup_eC2SecurityGroups,
    cacheSecurityGroup_cacheSecurityGroupName,

    -- * CacheSecurityGroupMembership
    CacheSecurityGroupMembership (..),
    newCacheSecurityGroupMembership,
    cacheSecurityGroupMembership_status,
    cacheSecurityGroupMembership_cacheSecurityGroupName,

    -- * CacheSubnetGroup
    CacheSubnetGroup (..),
    newCacheSubnetGroup,
    cacheSubnetGroup_cacheSubnetGroupName,
    cacheSubnetGroup_subnets,
    cacheSubnetGroup_arn,
    cacheSubnetGroup_cacheSubnetGroupDescription,
    cacheSubnetGroup_vpcId,
    cacheSubnetGroup_supportedNetworkTypes,

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
    customerNodeEndpoint_port,
    customerNodeEndpoint_address,

    -- * DestinationDetails
    DestinationDetails (..),
    newDestinationDetails,
    destinationDetails_kinesisFirehoseDetails,
    destinationDetails_cloudWatchLogsDetails,

    -- * EC2SecurityGroup
    EC2SecurityGroup (..),
    newEC2SecurityGroup,
    eC2SecurityGroup_eC2SecurityGroupOwnerId,
    eC2SecurityGroup_status,
    eC2SecurityGroup_eC2SecurityGroupName,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_port,
    endpoint_address,

    -- * EngineDefaults
    EngineDefaults (..),
    newEngineDefaults,
    engineDefaults_marker,
    engineDefaults_cacheNodeTypeSpecificParameters,
    engineDefaults_cacheParameterGroupFamily,
    engineDefaults_parameters,

    -- * Event
    Event (..),
    newEvent,
    event_message,
    event_date,
    event_sourceType,
    event_sourceIdentifier,

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
    globalReplicationGroup_transitEncryptionEnabled,
    globalReplicationGroup_globalReplicationGroupDescription,
    globalReplicationGroup_clusterEnabled,
    globalReplicationGroup_globalReplicationGroupId,
    globalReplicationGroup_members,
    globalReplicationGroup_arn,
    globalReplicationGroup_atRestEncryptionEnabled,
    globalReplicationGroup_status,
    globalReplicationGroup_cacheNodeType,
    globalReplicationGroup_authTokenEnabled,
    globalReplicationGroup_engine,
    globalReplicationGroup_engineVersion,
    globalReplicationGroup_globalNodeGroups,

    -- * GlobalReplicationGroupInfo
    GlobalReplicationGroupInfo (..),
    newGlobalReplicationGroupInfo,
    globalReplicationGroupInfo_globalReplicationGroupId,
    globalReplicationGroupInfo_globalReplicationGroupMemberRole,

    -- * GlobalReplicationGroupMember
    GlobalReplicationGroupMember (..),
    newGlobalReplicationGroupMember,
    globalReplicationGroupMember_replicationGroupRegion,
    globalReplicationGroupMember_automaticFailover,
    globalReplicationGroupMember_status,
    globalReplicationGroupMember_role,
    globalReplicationGroupMember_replicationGroupId,

    -- * KinesisFirehoseDestinationDetails
    KinesisFirehoseDestinationDetails (..),
    newKinesisFirehoseDestinationDetails,
    kinesisFirehoseDestinationDetails_deliveryStream,

    -- * LogDeliveryConfiguration
    LogDeliveryConfiguration (..),
    newLogDeliveryConfiguration,
    logDeliveryConfiguration_message,
    logDeliveryConfiguration_logType,
    logDeliveryConfiguration_destinationType,
    logDeliveryConfiguration_status,
    logDeliveryConfiguration_logFormat,
    logDeliveryConfiguration_destinationDetails,

    -- * LogDeliveryConfigurationRequest
    LogDeliveryConfigurationRequest (..),
    newLogDeliveryConfigurationRequest,
    logDeliveryConfigurationRequest_logType,
    logDeliveryConfigurationRequest_destinationType,
    logDeliveryConfigurationRequest_logFormat,
    logDeliveryConfigurationRequest_enabled,
    logDeliveryConfigurationRequest_destinationDetails,

    -- * NodeGroup
    NodeGroup (..),
    newNodeGroup,
    nodeGroup_primaryEndpoint,
    nodeGroup_status,
    nodeGroup_nodeGroupMembers,
    nodeGroup_readerEndpoint,
    nodeGroup_nodeGroupId,
    nodeGroup_slots,

    -- * NodeGroupConfiguration
    NodeGroupConfiguration (..),
    newNodeGroupConfiguration,
    nodeGroupConfiguration_replicaAvailabilityZones,
    nodeGroupConfiguration_replicaOutpostArns,
    nodeGroupConfiguration_primaryOutpostArn,
    nodeGroupConfiguration_primaryAvailabilityZone,
    nodeGroupConfiguration_nodeGroupId,
    nodeGroupConfiguration_slots,
    nodeGroupConfiguration_replicaCount,

    -- * NodeGroupMember
    NodeGroupMember (..),
    newNodeGroupMember,
    nodeGroupMember_preferredAvailabilityZone,
    nodeGroupMember_cacheClusterId,
    nodeGroupMember_preferredOutpostArn,
    nodeGroupMember_cacheNodeId,
    nodeGroupMember_readEndpoint,
    nodeGroupMember_currentRole,

    -- * NodeGroupMemberUpdateStatus
    NodeGroupMemberUpdateStatus (..),
    newNodeGroupMemberUpdateStatus,
    nodeGroupMemberUpdateStatus_nodeUpdateStatus,
    nodeGroupMemberUpdateStatus_nodeUpdateEndDate,
    nodeGroupMemberUpdateStatus_nodeUpdateStartDate,
    nodeGroupMemberUpdateStatus_nodeDeletionDate,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy,
    nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate,
    nodeGroupMemberUpdateStatus_cacheClusterId,
    nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate,
    nodeGroupMemberUpdateStatus_cacheNodeId,

    -- * NodeGroupUpdateStatus
    NodeGroupUpdateStatus (..),
    newNodeGroupUpdateStatus,
    nodeGroupUpdateStatus_nodeGroupMemberUpdateStatus,
    nodeGroupUpdateStatus_nodeGroupId,

    -- * NodeSnapshot
    NodeSnapshot (..),
    newNodeSnapshot,
    nodeSnapshot_snapshotCreateTime,
    nodeSnapshot_cacheClusterId,
    nodeSnapshot_cacheSize,
    nodeSnapshot_cacheNodeId,
    nodeSnapshot_nodeGroupId,
    nodeSnapshot_cacheNodeCreateTime,
    nodeSnapshot_nodeGroupConfiguration,

    -- * NotificationConfiguration
    NotificationConfiguration (..),
    newNotificationConfiguration,
    notificationConfiguration_topicStatus,
    notificationConfiguration_topicArn,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_changeType,
    parameter_parameterValue,
    parameter_isModifiable,
    parameter_description,
    parameter_parameterName,
    parameter_minimumEngineVersion,
    parameter_source,
    parameter_allowedValues,
    parameter_dataType,

    -- * ParameterNameValue
    ParameterNameValue (..),
    newParameterNameValue,
    parameterNameValue_parameterValue,
    parameterNameValue_parameterName,

    -- * PendingLogDeliveryConfiguration
    PendingLogDeliveryConfiguration (..),
    newPendingLogDeliveryConfiguration,
    pendingLogDeliveryConfiguration_logType,
    pendingLogDeliveryConfiguration_destinationType,
    pendingLogDeliveryConfiguration_logFormat,
    pendingLogDeliveryConfiguration_destinationDetails,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
    pendingModifiedValues_cacheNodeIdsToRemove,
    pendingModifiedValues_logDeliveryConfigurations,
    pendingModifiedValues_numCacheNodes,
    pendingModifiedValues_cacheNodeType,
    pendingModifiedValues_authTokenStatus,
    pendingModifiedValues_engineVersion,

    -- * ProcessedUpdateAction
    ProcessedUpdateAction (..),
    newProcessedUpdateAction,
    processedUpdateAction_updateActionStatus,
    processedUpdateAction_cacheClusterId,
    processedUpdateAction_serviceUpdateName,
    processedUpdateAction_replicationGroupId,

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
    replicationGroup_transitEncryptionEnabled,
    replicationGroup_memberClustersOutpostArns,
    replicationGroup_globalReplicationGroupInfo,
    replicationGroup_clusterEnabled,
    replicationGroup_autoMinorVersionUpgrade,
    replicationGroup_replicationGroupCreateTime,
    replicationGroup_automaticFailover,
    replicationGroup_arn,
    replicationGroup_snapshottingClusterId,
    replicationGroup_logDeliveryConfigurations,
    replicationGroup_atRestEncryptionEnabled,
    replicationGroup_ipDiscovery,
    replicationGroup_status,
    replicationGroup_cacheNodeType,
    replicationGroup_description,
    replicationGroup_authTokenEnabled,
    replicationGroup_nodeGroups,
    replicationGroup_snapshotWindow,
    replicationGroup_snapshotRetentionLimit,
    replicationGroup_userGroupIds,
    replicationGroup_kmsKeyId,
    replicationGroup_pendingModifiedValues,
    replicationGroup_dataTiering,
    replicationGroup_authTokenLastModifiedDate,
    replicationGroup_replicationGroupId,
    replicationGroup_memberClusters,
    replicationGroup_networkType,
    replicationGroup_multiAZ,
    replicationGroup_configurationEndpoint,

    -- * ReplicationGroupPendingModifiedValues
    ReplicationGroupPendingModifiedValues (..),
    newReplicationGroupPendingModifiedValues,
    replicationGroupPendingModifiedValues_resharding,
    replicationGroupPendingModifiedValues_automaticFailoverStatus,
    replicationGroupPendingModifiedValues_logDeliveryConfigurations,
    replicationGroupPendingModifiedValues_userGroups,
    replicationGroupPendingModifiedValues_authTokenStatus,
    replicationGroupPendingModifiedValues_primaryClusterId,

    -- * ReservedCacheNode
    ReservedCacheNode (..),
    newReservedCacheNode,
    reservedCacheNode_reservedCacheNodesOfferingId,
    reservedCacheNode_recurringCharges,
    reservedCacheNode_state,
    reservedCacheNode_offeringType,
    reservedCacheNode_cacheNodeCount,
    reservedCacheNode_cacheNodeType,
    reservedCacheNode_duration,
    reservedCacheNode_reservationARN,
    reservedCacheNode_reservedCacheNodeId,
    reservedCacheNode_productDescription,
    reservedCacheNode_fixedPrice,
    reservedCacheNode_startTime,
    reservedCacheNode_usagePrice,

    -- * ReservedCacheNodesOffering
    ReservedCacheNodesOffering (..),
    newReservedCacheNodesOffering,
    reservedCacheNodesOffering_reservedCacheNodesOfferingId,
    reservedCacheNodesOffering_recurringCharges,
    reservedCacheNodesOffering_offeringType,
    reservedCacheNodesOffering_cacheNodeType,
    reservedCacheNodesOffering_duration,
    reservedCacheNodesOffering_productDescription,
    reservedCacheNodesOffering_fixedPrice,
    reservedCacheNodesOffering_usagePrice,

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
    securityGroupMembership_securityGroupId,
    securityGroupMembership_status,

    -- * ServiceUpdate
    ServiceUpdate (..),
    newServiceUpdate,
    serviceUpdate_serviceUpdateType,
    serviceUpdate_serviceUpdateEndDate,
    serviceUpdate_serviceUpdateReleaseDate,
    serviceUpdate_estimatedUpdateTime,
    serviceUpdate_autoUpdateAfterRecommendedApplyByDate,
    serviceUpdate_serviceUpdateRecommendedApplyByDate,
    serviceUpdate_serviceUpdateSeverity,
    serviceUpdate_serviceUpdateName,
    serviceUpdate_serviceUpdateStatus,
    serviceUpdate_engine,
    serviceUpdate_serviceUpdateDescription,
    serviceUpdate_engineVersion,

    -- * SlotMigration
    SlotMigration (..),
    newSlotMigration,
    slotMigration_progressPercentage,

    -- * Snapshot
    Snapshot (..),
    newSnapshot,
    snapshot_port,
    snapshot_snapshotStatus,
    snapshot_cacheSubnetGroupName,
    snapshot_snapshotName,
    snapshot_snapshotSource,
    snapshot_autoMinorVersionUpgrade,
    snapshot_automaticFailover,
    snapshot_arn,
    snapshot_topicArn,
    snapshot_cacheClusterCreateTime,
    snapshot_numCacheNodes,
    snapshot_cacheNodeType,
    snapshot_cacheParameterGroupName,
    snapshot_preferredAvailabilityZone,
    snapshot_cacheClusterId,
    snapshot_snapshotWindow,
    snapshot_snapshotRetentionLimit,
    snapshot_replicationGroupDescription,
    snapshot_preferredOutpostArn,
    snapshot_kmsKeyId,
    snapshot_engine,
    snapshot_preferredMaintenanceWindow,
    snapshot_vpcId,
    snapshot_dataTiering,
    snapshot_replicationGroupId,
    snapshot_nodeSnapshots,
    snapshot_numNodeGroups,
    snapshot_engineVersion,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetOutpost,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,
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
    unprocessedUpdateAction_errorMessage,
    unprocessedUpdateAction_cacheClusterId,
    unprocessedUpdateAction_serviceUpdateName,
    unprocessedUpdateAction_replicationGroupId,
    unprocessedUpdateAction_errorType,

    -- * UpdateAction
    UpdateAction (..),
    newUpdateAction,
    updateAction_serviceUpdateType,
    updateAction_nodeGroupUpdateStatus,
    updateAction_serviceUpdateReleaseDate,
    updateAction_estimatedUpdateTime,
    updateAction_updateActionStatus,
    updateAction_updateActionAvailableDate,
    updateAction_cacheClusterId,
    updateAction_serviceUpdateRecommendedApplyByDate,
    updateAction_serviceUpdateSeverity,
    updateAction_serviceUpdateName,
    updateAction_serviceUpdateStatus,
    updateAction_engine,
    updateAction_slaMet,
    updateAction_replicationGroupId,
    updateAction_cacheNodeUpdateStatus,
    updateAction_nodesUpdated,
    updateAction_updateActionStatusModifiedDate,

    -- * UpdateActionResultsMessage
    UpdateActionResultsMessage (..),
    newUpdateActionResultsMessage,
    updateActionResultsMessage_unprocessedUpdateActions,
    updateActionResultsMessage_processedUpdateActions,

    -- * User
    User (..),
    newUser,
    user_accessString,
    user_authentication,
    user_userName,
    user_arn,
    user_status,
    user_minimumEngineVersion,
    user_userGroupIds,
    user_userId,
    user_engine,

    -- * UserGroup
    UserGroup (..),
    newUserGroup,
    userGroup_userGroupId,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_pendingChanges,
    userGroup_status,
    userGroup_minimumEngineVersion,
    userGroup_engine,
    userGroup_userIds,

    -- * UserGroupPendingChanges
    UserGroupPendingChanges (..),
    newUserGroupPendingChanges,
    userGroupPendingChanges_userIdsToRemove,
    userGroupPendingChanges_userIdsToAdd,

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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | A cache parameter group with the requested name already exists.
_CacheParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | A cache security group with the specified name already exists.
_CacheSecurityGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheSecurityGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes in a single cluster.
_NodeQuotaForClusterExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForClusterExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForClusterExceeded"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of clusters per customer.
_ClusterQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClusterQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "ClusterQuotaForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the maximum
-- number of snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The user group was not found or does not exist
_UserGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The specified replication group does not exist.
_ReplicationGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupNotFoundFault"
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

-- | The specified Amazon EC2 security group is already authorized for the
-- specified cache security group.
_AuthorizationAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested cache subnet group name does not refer to an existing
-- cache subnet group.
_CacheSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The service update doesn\'t exist
_ServiceUpdateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUpdateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceUpdateNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The user does not exist or could not be found.
_UserNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserNotFoundFault =
  Core._MatchServiceError
    defaultService
    "UserNotFound"
    Prelude.. Core.hasStatus 404

-- | The current state of the snapshot does not allow the requested operation
-- to occur.
_InvalidSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The requested cluster ID does not refer to an existing cluster.
_CacheClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheClusterNotFound"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the allowed
-- number of cache subnet groups.
_CacheSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | You already have a reservation with the given identifier.
_ReservedCacheNodeAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeAlreadyExists"
    Prelude.. Core.hasStatus 404

-- | The operation was not performed because no changes were required.
_NoOperationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoOperationFault =
  Core._MatchServiceError
    defaultService
    "NoOperationFault"
    Prelude.. Core.hasStatus 400

-- | A user with this ID already exists.
_UserAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The requested tag was not found on this resource.
_TagNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagNotFoundFault =
  Core._MatchServiceError
    defaultService
    "TagNotFound"
    Prelude.. Core.hasStatus 404

-- | The Global datastore is not available or in primary-only state.
_InvalidGlobalReplicationGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalReplicationGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalReplicationGroupState"
    Prelude.. Core.hasStatus 400

-- | You must add default user to a user group.
_DefaultUserRequired :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultUserRequired =
  Core._MatchServiceError
    defaultService
    "DefaultUserRequired"
    Prelude.. Core.hasStatus 400

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterCombinationException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterCombination"
    Prelude.. Core.hasStatus 400

-- | The specified service linked role (SLR) was not found.
_ServiceLinkedRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceLinkedRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ServiceLinkedRoleNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The specified Amazon EC2 security group is not authorized for the
-- specified cache security group.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The quota of users has been exceeded.
_UserQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The Global datastore does not exist
_GlobalReplicationGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalReplicationGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalReplicationGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The specified replication group already exists.
_ReplicationGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | A user with this username already exists.
_DuplicateUserNameFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateUserNameFault =
  Core._MatchServiceError
    defaultService
    "DuplicateUserName"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the maximum
-- number of cache security groups.
_CacheParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache security groups.
_CacheSecurityGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "QuotaExceeded.CacheSecurityGroup"
    Prelude.. Core.hasStatus 400

-- | The designated replication group is not available for data migration.
_ReplicationGroupNotUnderMigrationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupNotUnderMigrationFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupNotUnderMigrationFault"
    Prelude.. Core.hasStatus 400

-- | The default user assigned to the user group.
_DefaultUserAssociatedToUserGroupFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultUserAssociatedToUserGroupFault =
  Core._MatchServiceError
    defaultService
    "DefaultUserAssociatedToUserGroup"
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

-- | The requested cache parameter group name does not refer to an existing
-- cache parameter group.
_CacheParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidARNFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidARNFault =
  Core._MatchServiceError defaultService "InvalidARN"
    Prelude.. Core.hasStatus 400

-- | The Global datastore name already exists.
_GlobalReplicationGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalReplicationGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalReplicationGroupAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The requested cache security group name does not refer to an existing
-- cache security group.
_CacheSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CacheSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | You already have a snapshot with the given name.
_SnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "SnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The @TestFailover@ action is not available.
_TestFailoverNotAvailableFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TestFailoverNotAvailableFault =
  Core._MatchServiceError
    defaultService
    "TestFailoverNotAvailableFault"
    Prelude.. Core.hasStatus 400

-- | The requested cache node offering does not exist.
_ReservedCacheNodesOfferingNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodesOfferingNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodesOfferingNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested cache subnet group is currently in use.
_CacheSubnetGroupInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupInUse =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupInUse"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes per customer.
_NodeQuotaForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NodeQuotaForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "NodeQuotaForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The user group is not in an active state.
_InvalidUserGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidUserGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidUserGroupState"
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

-- | The current state of the cache security group does not allow deletion.
_InvalidCacheSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCacheSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is being used by another cache subnet group.
_SubnetInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetInUse =
  Core._MatchServiceError
    defaultService
    "SubnetInUse"
    Prelude.. Core.hasStatus 400

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

-- | The requested snapshot name does not refer to an existing snapshot.
_SnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The request cannot be processed because it would exceed the user\'s
-- cache node quota.
_ReservedCacheNodeQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested cache subnet group name is already in use by an existing
-- cache subnet group.
_CacheSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The user group with this ID already exists.
_UserGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "UserGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a cache subnet group.
_CacheSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "CacheSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The KMS key supplied is not valid.
_InvalidKMSKeyFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidKMSKeyFault =
  Core._MatchServiceError
    defaultService
    "InvalidKMSKeyFault"
    Prelude.. Core.hasStatus 400

-- | You already have a cluster with the given identifier.
_CacheClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CacheClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "CacheClusterAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The targeted replication group is not available.
_ReplicationGroupAlreadyUnderMigrationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplicationGroupAlreadyUnderMigrationFault =
  Core._MatchServiceError
    defaultService
    "ReplicationGroupAlreadyUnderMigrationFault"
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

-- | The requested cluster is not in the @available@ state.
_InvalidCacheClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCacheClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheClusterState"
    Prelude.. Core.hasStatus 400

-- | The current state of the cache parameter group does not allow the
-- requested operation to occur.
_InvalidCacheParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCacheParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidCacheParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValue"
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

-- | The customer has exceeded the allowed rate of API calls.
_APICallRateForCustomerExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_APICallRateForCustomerExceededFault =
  Core._MatchServiceError
    defaultService
    "APICallRateForCustomerExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested reserved cache node was not found.
_ReservedCacheNodeNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReservedCacheNodeNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ReservedCacheNodeNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested replication group is not in the @available@ state.
_InvalidReplicationGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReplicationGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidReplicationGroupState"
    Prelude.. Core.hasStatus 400

-- | The number of users exceeds the user group limit.
_UserGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UserGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "UserGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400
