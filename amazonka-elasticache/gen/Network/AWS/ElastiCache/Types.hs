{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types
    (
    -- * Service
      ElastiCache

    -- * Errors
    , _CacheSubnetGroupAlreadyExistsFault
    , _CacheSubnetGroupInUse
    , _CacheSecurityGroupNotFoundFault
    , _ReservedCacheNodeAlreadyExistsFault
    , _AuthorizationAlreadyExistsFault
    , _ReservedCacheNodeQuotaExceededFault
    , _CacheSubnetGroupQuotaExceededFault
    , _ReplicationGroupNotFoundFault
    , _ReservedCacheNodesOfferingNotFoundFault
    , _TagQuotaPerResourceExceeded
    , _InvalidSubnet
    , _SnapshotNotFoundFault
    , _InsufficientCacheClusterCapacityFault
    , _InvalidSnapshotStateFault
    , _SnapshotAlreadyExistsFault
    , _TagNotFoundFault
    , _SnapshotQuotaExceededFault
    , _CacheParameterGroupAlreadyExistsFault
    , _NodeQuotaForClusterExceededFault
    , _SnapshotFeatureNotSupportedFault
    , _CacheSubnetGroupNotFoundFault
    , _ReservedCacheNodeNotFoundFault
    , _InvalidParameterValueException
    , _InvalidVPCNetworkStateFault
    , _CacheClusterNotFoundFault
    , _InvalidReplicationGroupStateFault
    , _ReplicationGroupAlreadyExistsFault
    , _SubnetInUse
    , _CacheClusterAlreadyExistsFault
    , _ClusterQuotaForCustomerExceededFault
    , _AuthorizationNotFoundFault
    , _CacheSecurityGroupQuotaExceededFault
    , _InvalidCacheClusterStateFault
    , _CacheParameterGroupQuotaExceededFault
    , _NodeQuotaForCustomerExceededFault
    , _CacheSubnetQuotaExceededFault
    , _CacheParameterGroupNotFoundFault
    , _InvalidParameterCombinationException
    , _InvalidARNFault
    , _InvalidCacheParameterGroupStateFault
    , _CacheSecurityGroupAlreadyExistsFault
    , _InvalidCacheSecurityGroupStateFault

    -- * AZMode
    , AZMode (..)

    -- * AutomaticFailoverStatus
    , AutomaticFailoverStatus (..)

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
    , ccCacheNodeType
    , ccEngineVersion
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
    , ccCacheClusterStatus
    , ccPreferredAvailabilityZone
    , ccCacheParameterGroup
    , ccSnapshotRetentionLimit
    , ccReplicationGroupId
    , ccPendingModifiedValues
    , ccNumCacheNodes

    -- * CacheEngineVersion
    , CacheEngineVersion
    , cacheEngineVersion
    , cevCacheEngineDescription
    , cevCacheParameterGroupFamily
    , cevEngineVersion
    , cevCacheEngineVersionDescription
    , cevEngine

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
    , cntspAllowedValues
    , cntspDataType
    , cntspParameterName
    , cntspDescription

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
    , edParameters
    , edMarker

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
    , pAllowedValues
    , pDataType
    , pParameterName
    , pDescription

    -- * ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterValue
    , pnvParameterName

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
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
    , rgNodeGroups
    , rgStatus
    , rgSnapshottingClusterId
    , rgMemberClusters
    , rgReplicationGroupId
    , rgPendingModifiedValues
    , rgDescription
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
    , rcnProductDescription
    , rcnStartTime
    , rcnCacheNodeCount
    , rcnReservedCacheNodeId
    , rcnOfferingType
    , rcnUsagePrice
    , rcnRecurringCharges
    , rcnFixedPrice
    , rcnDuration
    , rcnReservedCacheNodesOfferingId

    -- * ReservedCacheNodesOffering
    , ReservedCacheNodesOffering
    , reservedCacheNodesOffering
    , rcnoCacheNodeType
    , rcnoProductDescription
    , rcnoOfferingType
    , rcnoUsagePrice
    , rcnoRecurringCharges
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
    , sCacheNodeType
    , sEngineVersion
    , sCacheClusterCreateTime
    , sAutoMinorVersionUpgrade
    , sCacheParameterGroupName
    , sSnapshotStatus
    , sSnapshotWindow
    , sVPCId
    , sCacheClusterId
    , sEngine
    , sPreferredMaintenanceWindow
    , sTopicARN
    , sCacheSubnetGroupName
    , sNodeSnapshots
    , sPreferredAvailabilityZone
    , sSnapshotRetentionLimit
    , sSnapshotName
    , sSnapshotSource
    , sNumCacheNodes
    , sPort

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
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2015-02-02@ of the Amazon ElastiCache SDK.
data ElastiCache

instance AWSService ElastiCache where
    type Sg ElastiCache = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "ElastiCache"
            , _svcPrefix = "elasticache"
            , _svcVersion = "2015-02-02"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70
            , _svcStatus = statusSuccess
            , _svcError = parseXMLError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The requested cache subnet group name is already in use by an existing
-- cache subnet group.
_CacheSubnetGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupAlreadyExists"

-- | The requested cache subnet group is currently in use.
_CacheSubnetGroupInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupInUse =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupInUse"

-- | The requested cache security group name does not refer to an existing
-- cache security group.
_CacheSecurityGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CacheSecurityGroupNotFound"

-- | You already have a reservation with the given identifier.
_ReservedCacheNodeAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeAlreadyExistsFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedCacheNodeAlreadyExists"

-- | The specified Amazon EC2 security group is already authorized for the
-- specified cache security group.
_AuthorizationAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "AuthorizationAlreadyExists"

-- | The request cannot be processed because it would exceed the user\'s
-- cache node quota.
_ReservedCacheNodeQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ReservedCacheNodeQuotaExceeded"

-- | The request cannot be processed because it would exceed the allowed
-- number of cache subnet groups.
_CacheSubnetGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupQuotaExceeded"

-- | The specified replication group does not exist.
_ReplicationGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReplicationGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReplicationGroupNotFoundFault"

-- | The requested cache node offering does not exist.
_ReservedCacheNodesOfferingNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodesOfferingNotFoundFault =
    _ServiceError .
    hasStatus 404 . hasCode "ReservedCacheNodesOfferingNotFound"

-- | The request cannot be processed because it would cause the resource to
-- have more than the allowed number of tags. The maximum number of tags
-- permitted on a resource is 10.
_TagQuotaPerResourceExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_TagQuotaPerResourceExceeded =
    _ServiceError . hasStatus 400 . hasCode "TagQuotaPerResourceExceeded"

-- | An invalid subnet identifier was specified.
_InvalidSubnet :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSubnet = _ServiceError . hasStatus 400 . hasCode "InvalidSubnet"

-- | The requested snapshot name does not refer to an existing snapshot.
_SnapshotNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "SnapshotNotFoundFault"

-- | The requested cache node type is not available in the specified
-- Availability Zone.
_InsufficientCacheClusterCapacityFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InsufficientCacheClusterCapacityFault =
    _ServiceError . hasStatus 400 . hasCode "InsufficientCacheClusterCapacity"

-- | The current state of the snapshot does not allow the requested action to
-- occur.
_InvalidSnapshotStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidSnapshotStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidSnapshotState"

-- | You already have a snapshot with the given name.
_SnapshotAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotAlreadyExistsFault"

-- | The requested tag was not found on this resource.
_TagNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_TagNotFoundFault = _ServiceError . hasStatus 404 . hasCode "TagNotFound"

-- | The request cannot be processed because it would exceed the maximum
-- number of snapshots.
_SnapshotQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotQuotaExceededFault"

-- | A cache parameter group with the requested name already exists.
_CacheParameterGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheParameterGroupAlreadyExists"

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes in a single cache cluster.
_NodeQuotaForClusterExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForClusterExceededFault =
    _ServiceError . hasStatus 400 . hasCode "NodeQuotaForClusterExceeded"

-- | You attempted one of the following actions:
--
-- -   Creating a snapshot of a Redis cache cluster running on a /t1.micro/
--     cache node.
--
-- -   Creating a snapshot of a cache cluster that is running Memcached
--     rather than Redis.
--
-- Neither of these are supported by ElastiCache.
_SnapshotFeatureNotSupportedFault :: AWSError a => Getting (First ServiceError) a ServiceError
_SnapshotFeatureNotSupportedFault =
    _ServiceError . hasStatus 400 . hasCode "SnapshotFeatureNotSupportedFault"

-- | The requested cache subnet group name does not refer to an existing
-- cache subnet group.
_CacheSubnetGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSubnetGroupNotFoundFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetGroupNotFoundFault"

-- | The requested reserved cache node was not found.
_ReservedCacheNodeNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReservedCacheNodeNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "ReservedCacheNodeNotFound"

-- | The value for a parameter is invalid.
_InvalidParameterValueException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterValueException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterValue"

-- | The VPC network is in an invalid state.
_InvalidVPCNetworkStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidVPCNetworkStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidVPCNetworkStateFault"

-- | The requested cache cluster ID does not refer to an existing cache
-- cluster.
_CacheClusterNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheClusterNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CacheClusterNotFound"

-- | The requested replication group is not in the /available/ state.
_InvalidReplicationGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidReplicationGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidReplicationGroupState"

-- | The specified replication group already exists.
_ReplicationGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ReplicationGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "ReplicationGroupAlreadyExists"

-- | The requested subnet is being used by another cache subnet group.
_SubnetInUse :: AWSError a => Getting (First ServiceError) a ServiceError
_SubnetInUse = _ServiceError . hasStatus 400 . hasCode "SubnetInUse"

-- | You already have a cache cluster with the given identifier.
_CacheClusterAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheClusterAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheClusterAlreadyExists"

-- | The request cannot be processed because it would exceed the allowed
-- number of cache clusters per customer.
_ClusterQuotaForCustomerExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_ClusterQuotaForCustomerExceededFault =
    _ServiceError . hasStatus 400 . hasCode "ClusterQuotaForCustomerExceeded"

-- | The specified Amazon EC2 security group is not authorized for the
-- specified cache security group.
_AuthorizationNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_AuthorizationNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "AuthorizationNotFound"

-- | The request cannot be processed because it would exceed the allowed
-- number of cache security groups.
_CacheSecurityGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "QuotaExceeded.CacheSecurityGroup"

-- | The requested cache cluster is not in the /available/ state.
_InvalidCacheClusterStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCacheClusterStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidCacheClusterState"

-- | The request cannot be processed because it would exceed the maximum
-- number of cache security groups.
_CacheParameterGroupQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "CacheParameterGroupQuotaExceeded"

-- | The request cannot be processed because it would exceed the allowed
-- number of cache nodes per customer.
_NodeQuotaForCustomerExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_NodeQuotaForCustomerExceededFault =
    _ServiceError . hasStatus 400 . hasCode "NodeQuotaForCustomerExceeded"

-- | The request cannot be processed because it would exceed the allowed
-- number of subnets in a cache subnet group.
_CacheSubnetQuotaExceededFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSubnetQuotaExceededFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSubnetQuotaExceededFault"

-- | The requested cache parameter group name does not refer to an existing
-- cache parameter group.
_CacheParameterGroupNotFoundFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheParameterGroupNotFoundFault =
    _ServiceError . hasStatus 404 . hasCode "CacheParameterGroupNotFound"

-- | Two or more incompatible parameters were specified.
_InvalidParameterCombinationException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidParameterCombinationException =
    _ServiceError . hasStatus 400 . hasCode "InvalidParameterCombination"

-- | The requested Amazon Resource Name (ARN) does not refer to an existing
-- resource.
_InvalidARNFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidARNFault = _ServiceError . hasStatus 400 . hasCode "InvalidARN"

-- | The current state of the cache parameter group does not allow the
-- requested action to occur.
_InvalidCacheParameterGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCacheParameterGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidCacheParameterGroupState"

-- | A cache security group with the specified name already exists.
_CacheSecurityGroupAlreadyExistsFault :: AWSError a => Getting (First ServiceError) a ServiceError
_CacheSecurityGroupAlreadyExistsFault =
    _ServiceError . hasStatus 400 . hasCode "CacheSecurityGroupAlreadyExists"

-- | The current state of the cache security group does not allow deletion.
_InvalidCacheSecurityGroupStateFault :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidCacheSecurityGroupStateFault =
    _ServiceError . hasStatus 400 . hasCode "InvalidCacheSecurityGroupState"
