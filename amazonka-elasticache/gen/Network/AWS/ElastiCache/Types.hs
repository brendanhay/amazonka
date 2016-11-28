{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , _CacheParameterGroupAlreadyExistsFault
    , _ReservedCacheNodeNotFoundFault
    , _CacheSubnetGroupNotFoundFault
    , _SnapshotFeatureNotSupportedFault
    , _InvalidParameterValueException
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
    , ccAutoMinorVersionUpgrade
    , ccSecurityGroups
    , ccNotificationConfiguration
    , ccSnapshotWindow
    , ccCacheClusterId
    , ccConfigurationEndpoint
    , ccEngine
    , ccCacheSecurityGroups
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
    , ngNodeGroupMembers
    , ngNodeGroupId

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
    , nsCacheNodeCreateTime
    , nsCacheNodeId
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
    , rgNodeGroups
    , rgSnapshottingClusterId
    , rgMemberClusters
    , rgDescription
    , rgReplicationGroupId
    , rgPendingModifiedValues
    , rgAutomaticFailover

    -- * ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues
    , replicationGroupPendingModifiedValues
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

    -- * SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmStatus
    , sgmSecurityGroupId

    -- * Snapshot
    , Snapshot
    , snapshot
    , sEngineVersion
    , sCacheNodeType
    , sCacheClusterCreateTime
    , sAutoMinorVersionUpgrade
    , sCacheParameterGroupName
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
    , sSnapshotRetentionLimit
    , sSnapshotName
    , sNumCacheNodes
    , sPort
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

import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.ElastiCache.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
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
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupInUse"

-- | You already have a reservation with the given identifier.
--
--
_ReservedCacheNodeAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeAlreadyExistsFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedCacheNodeAlreadyExists"

-- | The requested cache security group name does not refer to an existing cache security group.
--
--
_CacheSecurityGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CacheSecurityGroupNotFound"

-- | The requested cache subnet group name is already in use by an existing cache subnet group.
--
--
_CacheSubnetGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupAlreadyExists"

-- | The request cannot be processed because it would exceed the allowed number of cache subnet groups.
--
--
_CacheSubnetGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupQuotaExceeded"

-- | The specified Amazon EC2 security group is already authorized for the specified cache security group.
--
--
_AuthorizationAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationAlreadyExists"

-- | The request cannot be processed because it would exceed the user's cache node quota.
--
--
_ReservedCacheNodeQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ReservedCacheNodeQuotaExceeded"

-- | The requested cache node offering does not exist.
--
--
_ReservedCacheNodesOfferingNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodesOfferingNotFoundFault =
    _ServiceError .
    hasStatus 404 . hasCode "ReservedCacheNodesOfferingNotFound"

-- | The specified replication group does not exist.
--
--
_ReplicationGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReplicationGroupNotFoundFault"

-- | An invalid subnet identifier was specified.
--
--
_InvalidSubnet :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | The request cannot be processed because it would cause the resource to have more than the allowed number of tags. The maximum number of tags permitted on a resource is 10.
--
--
_TagQuotaPerResourceExceeded :: AsError a => Getting (First ServiceError) a ServiceError
_TagQuotaPerResourceExceeded =
    _ServiceError . hasStatus 400 . hasCode "TagQuotaPerResourceExceeded"

-- | The requested snapshot name does not refer to an existing snapshot.
--
--
_SnapshotNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SnapshotNotFoundFault"

-- | The requested cache node type is not available in the specified Availability Zone.
--
--
_InsufficientCacheClusterCapacityFault :: AsError a => Getting (First ServiceError) a ServiceError
_InsufficientCacheClusterCapacityFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientCacheClusterCapacity"

-- | The current state of the snapshot does not allow the requested action to occur.
--
--
_InvalidSnapshotStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSnapshotStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidSnapshotState"

-- | You already have a snapshot with the given name.
--
--
_SnapshotAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotAlreadyExistsFault"

-- | The requested tag was not found on this resource.
--
--
_TagNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_TagNotFoundFault = _ServiceError . hasStatus 404 . hasCode "TagNotFound"

-- | The request cannot be processed because it would exceed the maximum number of snapshots.
--
--
_SnapshotQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotQuotaExceededFault"

-- | The request cannot be processed because it would exceed the allowed number of cache nodes in a single cache cluster.
--
--
_NodeQuotaForClusterExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForClusterExceededFault =
    _ServiceError . hasStatus 400 . hasCode "NodeQuotaForClusterExceeded"

-- | A cache parameter group with the requested name already exists.
--
--
_CacheParameterGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheParameterGroupAlreadyExists"

-- | The requested reserved cache node was not found.
--
--
_ReservedCacheNodeNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedCacheNodeNotFound"

-- | The requested cache subnet group name does not refer to an existing cache subnet group.
--
--
_CacheSubnetGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupNotFoundFault"

-- | You attempted one of the following actions:
--
--
--     * Creating a snapshot of a Redis cache cluster running on a /t1.micro/ cache node.
--
--     * Creating a snapshot of a cache cluster that is running Memcached rather than Redis.
--
--
--
-- Neither of these are supported by ElastiCache.
--
_SnapshotFeatureNotSupportedFault :: AsError a => Getting (First ServiceError) a ServiceError
_SnapshotFeatureNotSupportedFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotFeatureNotSupportedFault"

-- | The value for a parameter is invalid.
--
--
_InvalidParameterValueException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValue"

-- | The requested replication group is not in the /available/ state.
--
--
_InvalidReplicationGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidReplicationGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidReplicationGroupState"

-- | The specified replication group already exists.
--
--
_ReplicationGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_ReplicationGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ReplicationGroupAlreadyExists"

-- | The VPC network is in an invalid state.
--
--
_InvalidVPCNetworkStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidVPCNetworkStateFault"

-- | The requested subnet is being used by another cache subnet group.
--
--
_SubnetInUse :: AsError a => Getting (First ServiceError) a ServiceError
_SubnetInUse = _ServiceError . hasStatus 400 . hasCode "SubnetInUse"

-- | The requested cache cluster ID does not refer to an existing cache cluster.
--
--
_CacheClusterNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheClusterNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CacheClusterNotFound"

-- | The request cannot be processed because it would exceed the allowed number of cache clusters per customer.
--
--
_ClusterQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaForCustomerExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterQuotaForCustomerExceeded"

-- | The specified Amazon EC2 security group is not authorized for the specified cache security group.
--
--
_AuthorizationNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "AuthorizationNotFound"

-- | The requested cache cluster is not in the /available/ state.
--
--
_InvalidCacheClusterStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCacheClusterStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidCacheClusterState"

-- | The request cannot be processed because it would exceed the allowed number of cache security groups.
--
--
_CacheSecurityGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "QuotaExceeded.CacheSecurityGroup"

-- | You already have a cache cluster with the given identifier.
--
--
_CacheClusterAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheClusterAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheClusterAlreadyExists"

-- | The request cannot be processed because it would exceed the maximum number of cache security groups.
--
--
_CacheParameterGroupQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "CacheParameterGroupQuotaExceeded"

-- | The request cannot be processed because it would exceed the allowed number of cache nodes per customer.
--
--
_NodeQuotaForCustomerExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForCustomerExceededFault =
    _ServiceError . hasStatus 400 . hasCode "NodeQuotaForCustomerExceeded"

-- | The request cannot be processed because it would exceed the allowed number of subnets in a cache subnet group.
--
--
_CacheSubnetQuotaExceededFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSubnetQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetQuotaExceededFault"

-- | The requested cache parameter group name does not refer to an existing cache parameter group.
--
--
_CacheParameterGroupNotFoundFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CacheParameterGroupNotFound"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing resource.
--
--
_InvalidARNFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidARNFault = _ServiceError . hasStatus 400 . hasCode "InvalidARN"

-- | The current state of the cache parameter group does not allow the requested action to occur.
--
--
_InvalidCacheParameterGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCacheParameterGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidCacheParameterGroupState"

-- | Two or more incompatible parameters were specified.
--
--
_InvalidParameterCombinationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterCombination"

-- | The current state of the cache security group does not allow deletion.
--
--
_InvalidCacheSecurityGroupStateFault :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCacheSecurityGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidCacheSecurityGroupState"

-- | A cache security group with the specified name already exists.
--
--
_CacheSecurityGroupAlreadyExistsFault :: AsError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSecurityGroupAlreadyExists"
