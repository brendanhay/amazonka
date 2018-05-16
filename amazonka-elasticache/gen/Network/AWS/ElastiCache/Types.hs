{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types
    (
    -- * Service Configuration
      elastiCache

    -- * Errors
    , _CacheSubnetGroupInUse
    , _ReservedCacheNodeAlreadyExistsFault
    , _CacheSecurityGroupNotFoundFault
    , _CacheSubnetGroupAlreadyExistsFault
    , _NodeGroupsPerReplicationGroupQuotaExceededFault
    , _CacheSubnetGroupQuotaExceededFault
    , _AuthorizationAlreadyExistsFault
    , _ReservedCacheNodeQuotaExceededFault
    , _ReservedCacheNodesOfferingNotFoundFault
    , _ReplicationGroupNotFoundFault
    , _InvalidSubnet
    , _TagQuotaPerResourceExceeded
    , _SnapshotNotFoundFault
    , _InsufficientCacheClusterCapacityFault
    , _InvalidSnapshotStateFault
    , _SnapshotAlreadyExistsFault
    , _TagNotFoundFault
    , _SnapshotQuotaExceededFault
    , _NodeQuotaForClusterExceededFault
    , _APICallRateForCustomerExceededFault
    , _NodeGroupNotFoundFault
    , _CacheParameterGroupAlreadyExistsFault
    , _ReservedCacheNodeNotFoundFault
    , _CacheSubnetGroupNotFoundFault
    , _SnapshotFeatureNotSupportedFault
    , _InvalidParameterValueException
    , _TestFailoverNotAvailableFault
    , _InvalidReplicationGroupStateFault
    , _ReplicationGroupAlreadyExistsFault
    , _InvalidVPCNetworkStateFault
    , _SubnetInUse
    , _CacheClusterNotFoundFault
    , _ClusterQuotaForCustomerExceededFault
    , _AuthorizationNotFoundFault
    , _InvalidCacheClusterStateFault
    , _CacheSecurityGroupQuotaExceededFault
    , _CacheClusterAlreadyExistsFault
    , _CacheParameterGroupQuotaExceededFault
    , _NodeQuotaForCustomerExceededFault
    , _CacheSubnetQuotaExceededFault
    , _CacheParameterGroupNotFoundFault
    , _InvalidARNFault
    , _InvalidCacheParameterGroupStateFault
    , _InvalidParameterCombinationException
    , _InvalidCacheSecurityGroupStateFault
    , _CacheSecurityGroupAlreadyExistsFault

    -- * AZMode
    , AZMode (..)

    -- * AutomaticFailoverStatus
    , AutomaticFailoverStatus (..)

    -- * ChangeType
    , ChangeType (..)

    -- * PendingAutomaticFailoverStatus
    , PendingAutomaticFailoverStatus (..)

    -- * SourceType
    , SourceType (..)

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * CacheCluster
    , CacheCluster
    , cacheCluster
    , ccEngineVersion
    , ccCacheNodeType
    , ccCacheNodes
    , ccCacheClusterCreateTime
    , ccAtRestEncryptionEnabled
    , ccAutoMinorVersionUpgrade
    , ccSecurityGroups
    , ccNotificationConfiguration
    , ccTransitEncryptionEnabled
    , ccSnapshotWindow
    , ccCacheClusterId
    , ccConfigurationEndpoint
    , ccEngine
    , ccCacheSecurityGroups
    , ccAuthTokenEnabled
    , ccClientDownloadLandingPage
    , ccPreferredMaintenanceWindow
    , ccCacheSubnetGroupName
    , ccPreferredAvailabilityZone
    , ccCacheParameterGroup
    , ccCacheClusterStatus
    , ccSnapshotRetentionLimit
    , ccReplicationGroupId
    , ccPendingModifiedValues
    , ccNumCacheNodes

    -- * CacheEngineVersion
    , CacheEngineVersion
    , cacheEngineVersion
    , cevEngineVersion
    , cevCacheParameterGroupFamily
    , cevCacheEngineDescription
    , cevEngine
    , cevCacheEngineVersionDescription

    -- * CacheNode
    , CacheNode
    , cacheNode
    , cnSourceCacheNodeId
    , cnParameterGroupStatus
    , cnCacheNodeCreateTime
    , cnCustomerAvailabilityZone
    , cnCacheNodeId
    , cnCacheNodeStatus
    , cnEndpoint

    -- * CacheNodeTypeSpecificParameter
    , CacheNodeTypeSpecificParameter
    , cacheNodeTypeSpecificParameter
    , cntspCacheNodeTypeSpecificValues
    , cntspMinimumEngineVersion
    , cntspSource
    , cntspIsModifiable
    , cntspDataType
    , cntspAllowedValues
    , cntspParameterName
    , cntspDescription
    , cntspChangeType

    -- * CacheNodeTypeSpecificValue
    , CacheNodeTypeSpecificValue
    , cacheNodeTypeSpecificValue
    , cntsvCacheNodeType
    , cntsvValue

    -- * CacheParameterGroup
    , CacheParameterGroup
    , cacheParameterGroup
    , cpgCacheParameterGroupFamily
    , cpgCacheParameterGroupName
    , cpgDescription

    -- * CacheParameterGroupNameMessage
    , CacheParameterGroupNameMessage
    , cacheParameterGroupNameMessage
    , cpgnmCacheParameterGroupName

    -- * CacheParameterGroupStatus
    , CacheParameterGroupStatus
    , cacheParameterGroupStatus
    , cpgsCacheParameterGroupName
    , cpgsCacheNodeIdsToReboot
    , cpgsParameterApplyStatus

    -- * CacheSecurityGroup
    , CacheSecurityGroup
    , cacheSecurityGroup
    , csgCacheSecurityGroupName
    , csgOwnerId
    , csgEC2SecurityGroups
    , csgDescription

    -- * CacheSecurityGroupMembership
    , CacheSecurityGroupMembership
    , cacheSecurityGroupMembership
    , csgmStatus
    , csgmCacheSecurityGroupName

    -- * CacheSubnetGroup
    , CacheSubnetGroup
    , cacheSubnetGroup
    , csgVPCId
    , csgSubnets
    , csgCacheSubnetGroupName
    , csgCacheSubnetGroupDescription

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edCacheParameterGroupFamily
    , edCacheNodeTypeSpecificParameters
    , edMarker
    , edParameters

    -- * Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eMessage

    -- * NodeGroup
    , NodeGroup
    , nodeGroup
    , ngStatus
    , ngPrimaryEndpoint
    , ngSlots
    , ngNodeGroupMembers
    , ngNodeGroupId

    -- * NodeGroupConfiguration
    , NodeGroupConfiguration
    , nodeGroupConfiguration
    , ngcSlots
    , ngcReplicaCount
    , ngcPrimaryAvailabilityZone
    , ngcReplicaAvailabilityZones

    -- * NodeGroupMember
    , NodeGroupMember
    , nodeGroupMember
    , ngmCacheClusterId
    , ngmCacheNodeId
    , ngmPreferredAvailabilityZone
    , ngmCurrentRole
    , ngmReadEndpoint

    -- * NodeSnapshot
    , NodeSnapshot
    , nodeSnapshot
    , nsNodeGroupConfiguration
    , nsCacheNodeCreateTime
    , nsCacheClusterId
    , nsCacheNodeId
    , nsNodeGroupId
    , nsSnapshotCreateTime
    , nsCacheSize

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicStatus
    , ncTopicARN

    -- * Parameter
    , Parameter
    , parameter
    , pParameterValue
    , pMinimumEngineVersion
    , pSource
    , pIsModifiable
    , pDataType
    , pAllowedValues
    , pParameterName
    , pDescription
    , pChangeType

    -- * ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterValue
    , pnvParameterName

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvCacheNodeType
    , pmvCacheNodeIdsToRemove
    , pmvNumCacheNodes

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- * ReplicationGroup
    , ReplicationGroup
    , replicationGroup
    , rgStatus
    , rgCacheNodeType
    , rgNodeGroups
    , rgSnapshottingClusterId
    , rgClusterEnabled
    , rgAtRestEncryptionEnabled
    , rgTransitEncryptionEnabled
    , rgSnapshotWindow
    , rgConfigurationEndpoint
    , rgAuthTokenEnabled
    , rgMemberClusters
    , rgSnapshotRetentionLimit
    , rgDescription
    , rgReplicationGroupId
    , rgPendingModifiedValues
    , rgAutomaticFailover

    -- * ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues
    , replicationGroupPendingModifiedValues
    , rgpmvResharding
    , rgpmvPrimaryClusterId
    , rgpmvAutomaticFailoverStatus

    -- * ReservedCacheNode
    , ReservedCacheNode
    , reservedCacheNode
    , rcnCacheNodeType
    , rcnState
    , rcnStartTime
    , rcnProductDescription
    , rcnCacheNodeCount
    , rcnReservedCacheNodeId
    , rcnRecurringCharges
    , rcnOfferingType
    , rcnUsagePrice
    , rcnFixedPrice
    , rcnDuration
    , rcnReservedCacheNodesOfferingId

    -- * ReservedCacheNodesOffering
    , ReservedCacheNodesOffering
    , reservedCacheNodesOffering
    , rcnoCacheNodeType
    , rcnoProductDescription
    , rcnoRecurringCharges
    , rcnoOfferingType
    , rcnoUsagePrice
    , rcnoFixedPrice
    , rcnoDuration
    , rcnoReservedCacheNodesOfferingId

    -- * ReshardingConfiguration
    , ReshardingConfiguration
    , reshardingConfiguration
    , rcPreferredAvailabilityZones

    -- * ReshardingStatus
    , ReshardingStatus
    , reshardingStatus
    , rsSlotMigration

    -- * SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmStatus
    , sgmSecurityGroupId

    -- * SlotMigration
    , SlotMigration
    , slotMigration
    , smProgressPercentage

    -- * Snapshot
    , Snapshot
    , snapshot
    , sEngineVersion
    , sCacheNodeType
    , sCacheClusterCreateTime
    , sAutoMinorVersionUpgrade
    , sCacheParameterGroupName
    , sReplicationGroupDescription
    , sVPCId
    , sSnapshotStatus
    , sSnapshotWindow
    , sCacheClusterId
    , sEngine
    , sPreferredMaintenanceWindow
    , sTopicARN
    , sNodeSnapshots
    , sCacheSubnetGroupName
    , sPreferredAvailabilityZone
    , sNumNodeGroups
    , sSnapshotRetentionLimit
    , sSnapshotName
    , sReplicationGroupId
    , sNumCacheNodes
    , sPort
    , sAutomaticFailover
    , sSnapshotSource

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- * TagListMessage
    , TagListMessage
    , tagListMessage
    , tlmTagList
    ) where

import Network.AWS.ElastiCache.Types.Product
import Network.AWS.ElastiCache.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-02-02@ of the Amazon ElastiCache SDK configuration.
elastiCache :: Service
elastiCache =
  Service
    { _svcAbbrev = "ElastiCache"
    , _svcSigner = v4
    , _svcPrefix = "elasticache"
    , _svcVersion = "2015-02-02"
    , _svcEndpoint = defaultEndpoint elastiCache
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseXMLError "ElastiCache"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The requested cache subnet group is currently in use.
--
--
_CacheSubnetGroupInUse :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupInUse =
  _MatchServiceError elastiCache "CacheSubnetGroupInUse" . hasStatus 400


-- | You already have a reservation with the given identifier.
--
--
_ReservedCacheNodeAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeAlreadyExistsFault =
  _MatchServiceError elastiCache "ReservedCacheNodeAlreadyExists" .
  hasStatus 404


-- | The requested cache security group name does not refer to an existing cache security group.
--
--
_CacheSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupNotFoundFault =
  _MatchServiceError elastiCache "CacheSecurityGroupNotFound" . hasStatus 404


-- | The requested cache subnet group name is already in use by an existing cache subnet group.
--
--
_CacheSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupAlreadyExistsFault =
  _MatchServiceError elastiCache "CacheSubnetGroupAlreadyExists" . hasStatus 400


-- | The request cannot be processed because it would exceed the maximum allowed number of node groups (shards) in a single replication group. The default maximum is 15
--
--
_NodeGroupsPerReplicationGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeGroupsPerReplicationGroupQuotaExceededFault =
  _MatchServiceError elastiCache "NodeGroupsPerReplicationGroupQuotaExceeded" .
  hasStatus 400


-- | The request cannot be processed because it would exceed the allowed number of cache subnet groups.
--
--
_CacheSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupQuotaExceededFault =
  _MatchServiceError elastiCache "CacheSubnetGroupQuotaExceeded" . hasStatus 400


-- | The specified Amazon EC2 security group is already authorized for the specified cache security group.
--
--
_AuthorizationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
  _MatchServiceError elastiCache "AuthorizationAlreadyExists" . hasStatus 400


-- | The request cannot be processed because it would exceed the user's cache node quota.
--
--
_ReservedCacheNodeQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeQuotaExceededFault =
  _MatchServiceError elastiCache "ReservedCacheNodeQuotaExceeded" .
  hasStatus 400


-- | The requested cache node offering does not exist.
--
--
_ReservedCacheNodesOfferingNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodesOfferingNotFoundFault =
  _MatchServiceError elastiCache "ReservedCacheNodesOfferingNotFound" .
  hasStatus 404


-- | The specified replication group does not exist.
--
--
_ReplicationGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationGroupNotFoundFault =
  _MatchServiceError elastiCache "ReplicationGroupNotFoundFault" . hasStatus 404


-- | An invalid subnet identifier was specified.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _MatchServiceError elastiCache "InvalidSubnet" . hasStatus 400


-- | The request cannot be processed because it would cause the resource to have more than the allowed number of tags. The maximum number of tags permitted on a resource is 50.
--
--
_TagQuotaPerResourceExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_TagQuotaPerResourceExceeded =
  _MatchServiceError elastiCache "TagQuotaPerResourceExceeded" . hasStatus 400


-- | The requested snapshot name does not refer to an existing snapshot.
--
--
_SnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotNotFoundFault =
  _MatchServiceError elastiCache "SnapshotNotFoundFault" . hasStatus 404


-- | The requested cache node type is not available in the specified Availability Zone.
--
--
_InsufficientCacheClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientCacheClusterCapacityFault =
  _MatchServiceError elastiCache "InsufficientCacheClusterCapacity" .
  hasStatus 400


-- | The current state of the snapshot does not allow the requested operation to occur.
--
--
_InvalidSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSnapshotStateFault =
  _MatchServiceError elastiCache "InvalidSnapshotState" . hasStatus 400


-- | You already have a snapshot with the given name.
--
--
_SnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotAlreadyExistsFault =
  _MatchServiceError elastiCache "SnapshotAlreadyExistsFault" . hasStatus 400


-- | The requested tag was not found on this resource.
--
--
_TagNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_TagNotFoundFault = _MatchServiceError elastiCache "TagNotFound" . hasStatus 404


-- | The request cannot be processed because it would exceed the maximum number of snapshots.
--
--
_SnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault =
  _MatchServiceError elastiCache "SnapshotQuotaExceededFault" . hasStatus 400


-- | The request cannot be processed because it would exceed the allowed number of cache nodes in a single cluster.
--
--
_NodeQuotaForClusterExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForClusterExceededFault =
  _MatchServiceError elastiCache "NodeQuotaForClusterExceeded" . hasStatus 400


-- | The customer has exceeded the allowed rate of API calls.
--
--
_APICallRateForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_APICallRateForCustomerExceededFault =
  _MatchServiceError elastiCache "APICallRateForCustomerExceeded" .
  hasStatus 400


-- | The node group specified by the @NodeGroupId@ parameter could not be found. Please verify that the node group exists and that you spelled the @NodeGroupId@ value correctly.
--
--
_NodeGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeGroupNotFoundFault =
  _MatchServiceError elastiCache "NodeGroupNotFoundFault" . hasStatus 404


-- | A cache parameter group with the requested name already exists.
--
--
_CacheParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupAlreadyExistsFault =
  _MatchServiceError elastiCache "CacheParameterGroupAlreadyExists" .
  hasStatus 400


-- | The requested reserved cache node was not found.
--
--
_ReservedCacheNodeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeNotFoundFault =
  _MatchServiceError elastiCache "ReservedCacheNodeNotFound" . hasStatus 404


-- | The requested cache subnet group name does not refer to an existing cache subnet group.
--
--
_CacheSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupNotFoundFault =
  _MatchServiceError elastiCache "CacheSubnetGroupNotFoundFault" . hasStatus 400


-- | You attempted one of the following operations:
--
--
--     * Creating a snapshot of a Redis cluster running on a @cache.t1.micro@ cache node.
--
--     * Creating a snapshot of a cluster that is running Memcached rather than Redis.
--
--
--
-- Neither of these are supported by ElastiCache.
--
_SnapshotFeatureNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotFeatureNotSupportedFault =
  _MatchServiceError elastiCache "SnapshotFeatureNotSupportedFault" .
  hasStatus 400


-- | The value for a parameter is invalid.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
  _MatchServiceError elastiCache "InvalidParameterValue" . hasStatus 400


-- | Prism for TestFailoverNotAvailableFault' errors.
_TestFailoverNotAvailableFault :: AsError a => Getting (First ServiceError) a ServiceError
_TestFailoverNotAvailableFault =
  _MatchServiceError elastiCache "TestFailoverNotAvailableFault" . hasStatus 400


-- | The requested replication group is not in the @available@ state.
--
--
_InvalidReplicationGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidReplicationGroupStateFault =
  _MatchServiceError elastiCache "InvalidReplicationGroupState" . hasStatus 400


-- | The specified replication group already exists.
--
--
_ReplicationGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationGroupAlreadyExistsFault =
  _MatchServiceError elastiCache "ReplicationGroupAlreadyExists" . hasStatus 400


-- | The VPC network is in an invalid state.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
  _MatchServiceError elastiCache "InvalidVPCNetworkStateFault" . hasStatus 400


-- | The requested subnet is being used by another cache subnet group.
--
--
_SubnetInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetInUse = _MatchServiceError elastiCache "SubnetInUse" . hasStatus 400


-- | The requested cluster ID does not refer to an existing cluster.
--
--
_CacheClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheClusterNotFoundFault =
  _MatchServiceError elastiCache "CacheClusterNotFound" . hasStatus 404


-- | The request cannot be processed because it would exceed the allowed number of clusters per customer.
--
--
_ClusterQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaForCustomerExceededFault =
  _MatchServiceError elastiCache "ClusterQuotaForCustomerExceeded" .
  hasStatus 400


-- | The specified Amazon EC2 security group is not authorized for the specified cache security group.
--
--
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
  _MatchServiceError elastiCache "AuthorizationNotFound" . hasStatus 404


-- | The requested cluster is not in the @available@ state.
--
--
_InvalidCacheClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCacheClusterStateFault =
  _MatchServiceError elastiCache "InvalidCacheClusterState" . hasStatus 400


-- | The request cannot be processed because it would exceed the allowed number of cache security groups.
--
--
_CacheSecurityGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupQuotaExceededFault =
  _MatchServiceError elastiCache "QuotaExceeded.CacheSecurityGroup" .
  hasStatus 400


-- | You already have a cluster with the given identifier.
--
--
_CacheClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheClusterAlreadyExistsFault =
  _MatchServiceError elastiCache "CacheClusterAlreadyExists" . hasStatus 400


-- | The request cannot be processed because it would exceed the maximum number of cache security groups.
--
--
_CacheParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupQuotaExceededFault =
  _MatchServiceError elastiCache "CacheParameterGroupQuotaExceeded" .
  hasStatus 400


-- | The request cannot be processed because it would exceed the allowed number of cache nodes per customer.
--
--
_NodeQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForCustomerExceededFault =
  _MatchServiceError elastiCache "NodeQuotaForCustomerExceeded" . hasStatus 400


-- | The request cannot be processed because it would exceed the allowed number of subnets in a cache subnet group.
--
--
_CacheSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetQuotaExceededFault =
  _MatchServiceError elastiCache "CacheSubnetQuotaExceededFault" . hasStatus 400


-- | The requested cache parameter group name does not refer to an existing cache parameter group.
--
--
_CacheParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupNotFoundFault =
  _MatchServiceError elastiCache "CacheParameterGroupNotFound" . hasStatus 404


-- | The requested Amazon Resource Name (ARN) does not refer to an existing resource.
--
--
_InvalidARNFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNFault = _MatchServiceError elastiCache "InvalidARN" . hasStatus 400


-- | The current state of the cache parameter group does not allow the requested operation to occur.
--
--
_InvalidCacheParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCacheParameterGroupStateFault =
  _MatchServiceError elastiCache "InvalidCacheParameterGroupState" .
  hasStatus 400


-- | Two or more incompatible parameters were specified.
--
--
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
  _MatchServiceError elastiCache "InvalidParameterCombination" . hasStatus 400


-- | The current state of the cache security group does not allow deletion.
--
--
_InvalidCacheSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCacheSecurityGroupStateFault =
  _MatchServiceError elastiCache "InvalidCacheSecurityGroupState" .
  hasStatus 400


-- | A cache security group with the specified name already exists.
--
--
_CacheSecurityGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupAlreadyExistsFault =
  _MatchServiceError elastiCache "CacheSecurityGroupAlreadyExists" .
  hasStatus 400

