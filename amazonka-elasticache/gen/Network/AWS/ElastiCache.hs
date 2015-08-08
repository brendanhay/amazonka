{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon ElastiCache
--
-- Amazon ElastiCache is a web service that makes it easier to set up,
-- operate, and scale a distributed cache in the cloud.
--
-- With ElastiCache, customers gain all of the benefits of a
-- high-performance, in-memory cache with far less of the administrative
-- burden of launching and managing a distributed cache. The service makes
-- setup, scaling, and cluster failure handling much simpler than in a
-- self-managed cache deployment.
--
-- In addition, through integration with Amazon CloudWatch, customers get
-- enhanced visibility into the key performance statistics associated with
-- their cache and can receive alarms if a part of their cache runs hot.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/Welcome.html AWS API Reference>
module Network.AWS.ElastiCache
    (
    -- * Service Description
      ElastiCache

    -- * Error Matchers
    -- $errors
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

    -- * Waiters
    -- $waiters

    -- ** CacheClusterAvailable
    , cacheClusterAvailable

    -- ** CacheClusterDeleted
    , cacheClusterDeleted

    -- ** ReplicationGroupDeleted
    , replicationGroupDeleted

    -- ** ReplicationGroupAvailable
    , replicationGroupAvailable

    -- * Operations
    -- $operations

    -- ** DeleteCacheSecurityGroup
    , module Network.AWS.ElastiCache.DeleteCacheSecurityGroup

    -- ** CreateReplicationGroup
    , module Network.AWS.ElastiCache.CreateReplicationGroup

    -- ** DeleteCacheCluster
    , module Network.AWS.ElastiCache.DeleteCacheCluster

    -- ** RebootCacheCluster
    , module Network.AWS.ElastiCache.RebootCacheCluster

    -- ** RevokeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress

    -- ** DescribeEvents (Paginated)
    , module Network.AWS.ElastiCache.DescribeEvents
    -- $pager

    -- ** DescribeEngineDefaultParameters (Paginated)
    , module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
    -- $pager

    -- ** ModifyCacheParameterGroup
    , module Network.AWS.ElastiCache.ModifyCacheParameterGroup

    -- ** CreateCacheCluster
    , module Network.AWS.ElastiCache.CreateCacheCluster

    -- ** ListTagsForResource
    , module Network.AWS.ElastiCache.ListTagsForResource

    -- ** DeleteReplicationGroup
    , module Network.AWS.ElastiCache.DeleteReplicationGroup

    -- ** PurchaseReservedCacheNodesOffering
    , module Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering

    -- ** DescribeCacheClusters (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheClusters
    -- $pager

    -- ** ModifyReplicationGroup
    , module Network.AWS.ElastiCache.ModifyReplicationGroup

    -- ** RemoveTagsFromResource
    , module Network.AWS.ElastiCache.RemoveTagsFromResource

    -- ** DescribeCacheParameters (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheParameters
    -- $pager

    -- ** DescribeCacheSubnetGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheSubnetGroups
    -- $pager

    -- ** CreateCacheSecurityGroup
    , module Network.AWS.ElastiCache.CreateCacheSecurityGroup

    -- ** AddTagsToResource
    , module Network.AWS.ElastiCache.AddTagsToResource

    -- ** AuthorizeCacheSecurityGroupIngress
    , module Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress

    -- ** CopySnapshot
    , module Network.AWS.ElastiCache.CopySnapshot

    -- ** CreateCacheSubnetGroup
    , module Network.AWS.ElastiCache.CreateCacheSubnetGroup

    -- ** DescribeCacheParameterGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheParameterGroups
    -- $pager

    -- ** ResetCacheParameterGroup
    , module Network.AWS.ElastiCache.ResetCacheParameterGroup

    -- ** DescribeSnapshots (Paginated)
    , module Network.AWS.ElastiCache.DescribeSnapshots
    -- $pager

    -- ** DescribeReservedCacheNodesOfferings (Paginated)
    , module Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
    -- $pager

    -- ** DeleteSnapshot
    , module Network.AWS.ElastiCache.DeleteSnapshot

    -- ** DescribeReplicationGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeReplicationGroups
    -- $pager

    -- ** ModifyCacheSubnetGroup
    , module Network.AWS.ElastiCache.ModifyCacheSubnetGroup

    -- ** CreateSnapshot
    , module Network.AWS.ElastiCache.CreateSnapshot

    -- ** DescribeCacheSecurityGroups (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
    -- $pager

    -- ** DeleteCacheParameterGroup
    , module Network.AWS.ElastiCache.DeleteCacheParameterGroup

    -- ** DescribeReservedCacheNodes (Paginated)
    , module Network.AWS.ElastiCache.DescribeReservedCacheNodes
    -- $pager

    -- ** DescribeCacheEngineVersions (Paginated)
    , module Network.AWS.ElastiCache.DescribeCacheEngineVersions
    -- $pager

    -- ** ModifyCacheCluster
    , module Network.AWS.ElastiCache.ModifyCacheCluster

    -- ** CreateCacheParameterGroup
    , module Network.AWS.ElastiCache.CreateCacheParameterGroup

    -- ** DeleteCacheSubnetGroup
    , module Network.AWS.ElastiCache.DeleteCacheSubnetGroup

    -- * Types

    -- ** AZMode
    , AZMode (..)

    -- ** AutomaticFailoverStatus
    , AutomaticFailoverStatus (..)

    -- ** PendingAutomaticFailoverStatus
    , PendingAutomaticFailoverStatus (..)

    -- ** SourceType
    , SourceType (..)

    -- ** AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- ** CacheCluster
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

    -- ** CacheEngineVersion
    , CacheEngineVersion
    , cacheEngineVersion
    , cevCacheEngineDescription
    , cevCacheParameterGroupFamily
    , cevEngineVersion
    , cevCacheEngineVersionDescription
    , cevEngine

    -- ** CacheNode
    , CacheNode
    , cacheNode
    , cnSourceCacheNodeId
    , cnParameterGroupStatus
    , cnCacheNodeCreateTime
    , cnCustomerAvailabilityZone
    , cnCacheNodeId
    , cnCacheNodeStatus
    , cnEndpoint

    -- ** CacheNodeTypeSpecificParameter
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

    -- ** CacheNodeTypeSpecificValue
    , CacheNodeTypeSpecificValue
    , cacheNodeTypeSpecificValue
    , cntsvCacheNodeType
    , cntsvValue

    -- ** CacheParameterGroup
    , CacheParameterGroup
    , cacheParameterGroup
    , cpgCacheParameterGroupFamily
    , cpgCacheParameterGroupName
    , cpgDescription

    -- ** CacheParameterGroupNameMessage
    , CacheParameterGroupNameMessage
    , cacheParameterGroupNameMessage
    , cpgnmCacheParameterGroupName

    -- ** CacheParameterGroupStatus
    , CacheParameterGroupStatus
    , cacheParameterGroupStatus
    , cpgsCacheParameterGroupName
    , cpgsCacheNodeIdsToReboot
    , cpgsParameterApplyStatus

    -- ** CacheSecurityGroup
    , CacheSecurityGroup
    , cacheSecurityGroup
    , csgCacheSecurityGroupName
    , csgOwnerId
    , csgEC2SecurityGroups
    , csgDescription

    -- ** CacheSecurityGroupMembership
    , CacheSecurityGroupMembership
    , cacheSecurityGroupMembership
    , csgmStatus
    , csgmCacheSecurityGroupName

    -- ** CacheSubnetGroup
    , CacheSubnetGroup
    , cacheSubnetGroup
    , csgVPCId
    , csgSubnets
    , csgCacheSubnetGroupName
    , csgCacheSubnetGroupDescription

    -- ** EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , esgStatus
    , esgEC2SecurityGroupOwnerId
    , esgEC2SecurityGroupName

    -- ** Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- ** EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edCacheParameterGroupFamily
    , edCacheNodeTypeSpecificParameters
    , edParameters
    , edMarker

    -- ** Event
    , Event
    , event
    , eSourceType
    , eSourceIdentifier
    , eDate
    , eMessage

    -- ** NodeGroup
    , NodeGroup
    , nodeGroup
    , ngStatus
    , ngPrimaryEndpoint
    , ngNodeGroupMembers
    , ngNodeGroupId

    -- ** NodeGroupMember
    , NodeGroupMember
    , nodeGroupMember
    , ngmCacheClusterId
    , ngmCacheNodeId
    , ngmPreferredAvailabilityZone
    , ngmCurrentRole
    , ngmReadEndpoint

    -- ** NodeSnapshot
    , NodeSnapshot
    , nodeSnapshot
    , nsCacheNodeCreateTime
    , nsCacheNodeId
    , nsSnapshotCreateTime
    , nsCacheSize

    -- ** NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicStatus
    , ncTopicARN

    -- ** Parameter
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

    -- ** ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterValue
    , pnvParameterName

    -- ** PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvEngineVersion
    , pmvCacheNodeIdsToRemove
    , pmvNumCacheNodes

    -- ** RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeFrequency
    , rcRecurringChargeAmount

    -- ** ReplicationGroup
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

    -- ** ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues
    , replicationGroupPendingModifiedValues
    , rgpmvPrimaryClusterId
    , rgpmvAutomaticFailoverStatus

    -- ** ReservedCacheNode
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

    -- ** ReservedCacheNodesOffering
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

    -- ** SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmStatus
    , sgmSecurityGroupId

    -- ** Snapshot
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

    -- ** Subnet
    , Subnet
    , subnet
    , sSubnetIdentifier
    , sSubnetAvailabilityZone

    -- ** Tag
    , Tag
    , tag
    , tagValue
    , tagKey

    -- ** TagListMessage
    , TagListMessage
    , tagListMessage
    , tlmTagList
    ) where

import           Network.AWS.ElastiCache.AddTagsToResource
import           Network.AWS.ElastiCache.AuthorizeCacheSecurityGroupIngress
import           Network.AWS.ElastiCache.CopySnapshot
import           Network.AWS.ElastiCache.CreateCacheCluster
import           Network.AWS.ElastiCache.CreateCacheParameterGroup
import           Network.AWS.ElastiCache.CreateCacheSecurityGroup
import           Network.AWS.ElastiCache.CreateCacheSubnetGroup
import           Network.AWS.ElastiCache.CreateReplicationGroup
import           Network.AWS.ElastiCache.CreateSnapshot
import           Network.AWS.ElastiCache.DeleteCacheCluster
import           Network.AWS.ElastiCache.DeleteCacheParameterGroup
import           Network.AWS.ElastiCache.DeleteCacheSecurityGroup
import           Network.AWS.ElastiCache.DeleteCacheSubnetGroup
import           Network.AWS.ElastiCache.DeleteReplicationGroup
import           Network.AWS.ElastiCache.DeleteSnapshot
import           Network.AWS.ElastiCache.DescribeCacheClusters
import           Network.AWS.ElastiCache.DescribeCacheEngineVersions
import           Network.AWS.ElastiCache.DescribeCacheParameterGroups
import           Network.AWS.ElastiCache.DescribeCacheParameters
import           Network.AWS.ElastiCache.DescribeCacheSecurityGroups
import           Network.AWS.ElastiCache.DescribeCacheSubnetGroups
import           Network.AWS.ElastiCache.DescribeEngineDefaultParameters
import           Network.AWS.ElastiCache.DescribeEvents
import           Network.AWS.ElastiCache.DescribeReplicationGroups
import           Network.AWS.ElastiCache.DescribeReservedCacheNodes
import           Network.AWS.ElastiCache.DescribeReservedCacheNodesOfferings
import           Network.AWS.ElastiCache.DescribeSnapshots
import           Network.AWS.ElastiCache.ListTagsForResource
import           Network.AWS.ElastiCache.ModifyCacheCluster
import           Network.AWS.ElastiCache.ModifyCacheParameterGroup
import           Network.AWS.ElastiCache.ModifyCacheSubnetGroup
import           Network.AWS.ElastiCache.ModifyReplicationGroup
import           Network.AWS.ElastiCache.PurchaseReservedCacheNodesOffering
import           Network.AWS.ElastiCache.RebootCacheCluster
import           Network.AWS.ElastiCache.RemoveTagsFromResource
import           Network.AWS.ElastiCache.ResetCacheParameterGroup
import           Network.AWS.ElastiCache.RevokeCacheSecurityGroupIngress
import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'ElastiCache'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}

{- $pager
This operation can return paginated results.
-}
