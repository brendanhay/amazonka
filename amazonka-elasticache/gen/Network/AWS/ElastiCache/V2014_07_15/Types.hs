{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | ElastiCache is a web service that makes it easy to deploy, operate, and
-- scale an in-memory cache in the cloud. The service improves the performance
-- of web applications by allowing you to retrieve information from fast,
-- managed, in-memory caches, instead of relying entirely on slower disk-based
-- databases. Amazon ElastiCache automatically detects and replaces failed
-- nodes, reducing the overhead associated with self-managed infrastructures
-- and provides a resilient system that mitigates the risk of overloaded
-- databases, which slow website and application load times. Through
-- integration with Amazon CloudWatch, Amazon ElastiCache provides enhanced
-- visibility into key performance metrics associated with your Memcached or
-- Redis nodes.
module Network.AWS.ElastiCache.V2014_07_15.Types
    (
    -- * Service
      ElastiCache
    -- ** Errors
    , Er (..)
    -- ** XML
    , xmlOptions

    -- * SourceType
    , SourceType (..)

    -- * AvailabilityZone
    , AvailabilityZone (..)
    , azName

    -- * ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues (..)
    , rgpmvPrimaryClusterId

    -- * CacheCluster
    , CacheCluster (..)
    , ccCacheClusterId
    , ccConfigurationEndpoint
    , ccClientDownloadLandingPage
    , ccCacheNodeType
    , ccEngine
    , ccEngineVersion
    , ccCacheClusterStatus
    , ccNumCacheNodes
    , ccPreferredAvailabilityZone
    , ccCacheClusterCreateTime
    , ccPreferredMaintenanceWindow
    , ccPendingModifiedValues
    , ccNotificationConfiguration
    , ccCacheSecurityGroups
    , ccCacheParameterGroup
    , ccCacheSubnetGroupName
    , ccCacheNodes
    , ccAutoMinorVersionUpgrade
    , ccSecurityGroups
    , ccReplicationGroupId
    , ccSnapshotRetentionLimit
    , ccSnapshotWindow

    -- * CacheEngineVersion
    , CacheEngineVersion (..)
    , cevEngine
    , cevEngineVersion
    , cevCacheParameterGroupFamily
    , cevCacheEngineDescription
    , cevCacheEngineVersionDescription

    -- * CacheNode
    , CacheNode (..)
    , cnCacheNodeId
    , cnCacheNodeStatus
    , cnCacheNodeCreateTime
    , cnEndpoint
    , cnParameterGroupStatus
    , cnSourceCacheNodeId
    , cnCustomerAvailabilityZone

    -- * CacheNodeTypeSpecificParameter
    , CacheNodeTypeSpecificParameter (..)
    , cntspParameterName
    , cntspDescription
    , cntspSource
    , cntspDataType
    , cntspAllowedValues
    , cntspIsModifiable
    , cntspMinimumEngineVersion
    , cntspCacheNodeTypeSpecificValues

    -- * CacheNodeTypeSpecificValue
    , CacheNodeTypeSpecificValue (..)
    , cntsvCacheNodeType
    , cntsvValue

    -- * CacheParameterGroup
    , CacheParameterGroup (..)
    , cpgCacheParameterGroupName
    , cpgCacheParameterGroupFamily
    , cpgDescription

    -- * CacheParameterGroupStatus
    , CacheParameterGroupStatus (..)
    , cpgsCacheParameterGroupName
    , cpgsParameterApplyStatus
    , cpgsCacheNodeIdsToReboot

    -- * CacheSecurityGroup
    , CacheSecurityGroup (..)
    , csgOwnerId
    , csgCacheSecurityGroupName
    , csgDescription
    , csgEC2SecurityGroups

    -- * CacheSecurityGroupMembership
    , CacheSecurityGroupMembership (..)
    , csgmCacheSecurityGroupName
    , csgmStatus

    -- * CacheSubnetGroup
    , CacheSubnetGroup (..)
    , csiCacheSubnetGroupName
    , csiCacheSubnetGroupDescription
    , csiVpcId
    , csiSubnets

    -- * EC2SecurityGroup
    , EC2SecurityGroup (..)
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId

    -- * Endpoint
    , Endpoint (..)
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults (..)
    , edCacheParameterGroupFamily
    , edMarker
    , edParameters
    , edCacheNodeTypeSpecificParameters

    -- * Event
    , Event (..)
    , exSourceIdentifier
    , exSourceType
    , exMessage
    , exDate

    -- * NodeGroup
    , NodeGroup (..)
    , ngNodeGroupId
    , ngStatus
    , ngPrimaryEndpoint
    , ngNodeGroupMembers

    -- * NodeGroupMember
    , NodeGroupMember (..)
    , ngmCacheClusterId
    , ngmCacheNodeId
    , ngmReadEndpoint
    , ngmPreferredAvailabilityZone
    , ngmCurrentRole

    -- * NodeSnapshot
    , NodeSnapshot (..)
    , nsCacheNodeId
    , nsCacheSize
    , nsCacheNodeCreateTime
    , nsSnapshotCreateTime

    -- * NotificationConfiguration
    , NotificationConfiguration (..)
    , ncTopicArn
    , ncTopicStatus

    -- * Parameter
    , Parameter (..)
    , prParameterName
    , prParameterValue
    , prDescription
    , prSource
    , prDataType
    , prAllowedValues
    , prIsModifiable
    , prMinimumEngineVersion

    -- * ParameterNameValue
    , ParameterNameValue (..)
    , pnvParameterName
    , pnvParameterValue

    -- * PendingModifiedValues
    , PendingModifiedValues (..)
    , pmvNumCacheNodes
    , pmvCacheNodeIdsToRemove
    , pmvEngineVersion

    -- * RecurringCharge
    , RecurringCharge (..)
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReplicationGroup
    , ReplicationGroup (..)
    , rgReplicationGroupId
    , rgDescription
    , rgStatus
    , rgPendingModifiedValues
    , rgMemberClusters
    , rgNodeGroups
    , rgSnapshottingClusterId

    -- * ReservedCacheNode
    , ReservedCacheNode (..)
    , rcnReservedCacheNodeId
    , rcnReservedCacheNodesOfferingId
    , rcnCacheNodeType
    , rcnStartTime
    , rcnDuration
    , rcnFixedPrice
    , rcnUsagePrice
    , rcnCacheNodeCount
    , rcnProductDescription
    , rcnOfferingType
    , rcnState
    , rcnRecurringCharges

    -- * ReservedCacheNodesOffering
    , ReservedCacheNodesOffering (..)
    , rcnoReservedCacheNodesOfferingId
    , rcnoCacheNodeType
    , rcnoDuration
    , rcnoFixedPrice
    , rcnoUsagePrice
    , rcnoProductDescription
    , rcnoOfferingType
    , rcnoRecurringCharges

    -- * SecurityGroupMembership
    , SecurityGroupMembership (..)
    , sgmSecurityGroupId
    , sgmStatus

    -- * Snapshot
    , Snapshot (..)
    , stSnapshotName
    , stCacheClusterId
    , stSnapshotStatus
    , stSnapshotSource
    , stCacheNodeType
    , stEngine
    , stEngineVersion
    , stNumCacheNodes
    , stPreferredAvailabilityZone
    , stCacheClusterCreateTime
    , stPreferredMaintenanceWindow
    , stTopicArn
    , stPort
    , stCacheParameterGroupName
    , stCacheSubnetGroupName
    , stVpcId
    , stAutoMinorVersionUpgrade
    , stSnapshotRetentionLimit
    , stSnapshotWindow
    , stNodeSnapshots

    -- * Subnet
    , Subnet (..)
    , sssssuSubnetIdentifier
    , sssssuSubnetAvailabilityZone

    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-07-15@) of the
-- @Amazon ElastiCache@ service.
data ElastiCache deriving (Typeable)

instance AWSService ElastiCache where
    type Sg ElastiCache = V4
    data Er ElastiCache
        = AuthorizationAlreadyExistsFault
        | AuthorizationNotFoundFault
        | CacheClusterAlreadyExistsFault
        | CacheClusterNotFoundFault
        | CacheParameterGroupAlreadyExistsFault
        | CacheParameterGroupNotFoundFault
        | CacheParameterGroupQuotaExceededFault
        | CacheSecurityGroupAlreadyExistsFault
        | CacheSecurityGroupNotFoundFault
        | CacheSecurityGroupQuotaExceededFault
        | CacheSubnetGroupAlreadyExistsFault
        | CacheSubnetGroupInUse
        | CacheSubnetGroupNotFoundFault
        | CacheSubnetGroupQuotaExceededFault
        | CacheSubnetQuotaExceededFault
        | ClusterQuotaForCustomerExceededFault
        | ElastiCacheClient HttpException
        | ElastiCacheSerializer String
        | ElastiCacheService String
        | InsufficientCacheClusterCapacityFault
        | InvalidCacheClusterStateFault
        | InvalidCacheParameterGroupStateFault
        | InvalidCacheSecurityGroupStateFault
        | InvalidParameterCombinationException
            { _ipceMessage :: Maybe Text
            }
        | InvalidParameterValueException
            { _ipveMessage :: Maybe Text
            }
        | InvalidReplicationGroupStateFault
        | InvalidSnapshotStateFault
        | InvalidSubnet
        | InvalidVPCNetworkStateFault
        | NodeQuotaForClusterExceededFault
        | NodeQuotaForCustomerExceededFault
        | ReplicationGroupAlreadyExistsFault
        | ReplicationGroupNotFoundFault
        | ReservedCacheNodeAlreadyExistsFault
        | ReservedCacheNodeNotFoundFault
        | ReservedCacheNodeQuotaExceededFault
        | ReservedCacheNodesOfferingNotFoundFault
        | SnapshotAlreadyExistsFault
        | SnapshotFeatureNotSupportedFault
        | SnapshotNotFoundFault
        | SnapshotQuotaExceededFault
        | SubnetInUse

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticache"
        , _svcVersion  = "2014-07-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er ElastiCache)
deriving instance Generic (Er ElastiCache)

instance AWSError (Er ElastiCache) where
    awsError = const "ElastiCacheError"

instance AWSServiceError (Er ElastiCache) where
    serviceError    = ElastiCacheService
    clientError     = ElastiCacheClient
    serializerError = ElastiCacheSerializer

instance Exception (Er ElastiCache)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://elasticache.amazonaws.com/doc/2014-07-15/"
    }

-- | The event source to retrieve events for. If no value is specified, all
-- events are returned. Valid values are: cache-cluster |
-- cache-parameter-group | cache-security-group | cache-subnet-group.
data SourceType
    = SourceTypeCacheCluster -- ^ cache-cluster
    | SourceTypeCacheParameterGroup -- ^ cache-parameter-group
    | SourceTypeCacheSecurityGroup -- ^ cache-security-group
    | SourceTypeCacheSubnetGroup -- ^ cache-subnet-group
      deriving (Eq, Show, Generic)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "cache-cluster" SourceTypeCacheCluster
         <|> match "cache-parameter-group" SourceTypeCacheParameterGroup
         <|> match "cache-security-group" SourceTypeCacheSecurityGroup
         <|> match "cache-subnet-group" SourceTypeCacheSubnetGroup

instance ToText SourceType where
    toText SourceTypeCacheCluster = "cache-cluster"
    toText SourceTypeCacheParameterGroup = "cache-parameter-group"
    toText SourceTypeCacheSecurityGroup = "cache-security-group"
    toText SourceTypeCacheSubnetGroup = "cache-subnet-group"

instance ToByteString SourceType

instance FromXML SourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceType"

instance ToQuery SourceType where
    toQuery = genericQuery def

-- | The Availability Zone associated with the subnet.
newtype AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
      -- ^ The name of the Availability Zone.
    } deriving (Show, Generic)

-- | The name of the Availability Zone.
azName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AvailabilityZone
    -> f AvailabilityZone
azName f x =
    (\y -> x { _azName = y })
       <$> f (_azName x)
{-# INLINE azName #-}

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericQuery def

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
newtype ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { _rgpmvPrimaryClusterId :: Maybe Text
      -- ^ The primary cluster ID which will be applied immediately (if
      -- --apply-immediately was specified), or during the next
      -- maintenance window.
    } deriving (Show, Generic)

-- | The primary cluster ID which will be applied immediately (if
-- --apply-immediately was specified), or during the next maintenance window.
rgpmvPrimaryClusterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReplicationGroupPendingModifiedValues
    -> f ReplicationGroupPendingModifiedValues
rgpmvPrimaryClusterId f x =
    (\y -> x { _rgpmvPrimaryClusterId = y })
       <$> f (_rgpmvPrimaryClusterId x)
{-# INLINE rgpmvPrimaryClusterId #-}

instance FromXML ReplicationGroupPendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroupPendingModifiedValues"

instance ToQuery ReplicationGroupPendingModifiedValues where
    toQuery = genericQuery def

-- | Contains all of the attributes of a specific cache cluster.
data CacheCluster = CacheCluster
    { _ccCacheClusterId :: Maybe Text
      -- ^ The user-supplied identifier of the cache cluster. This is a
      -- unique key that identifies a cache cluster.
    , _ccConfigurationEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to
      -- connect to a cache node.
    , _ccClientDownloadLandingPage :: Maybe Text
      -- ^ The URL of the web page where you can download the latest
      -- ElastiCache client library.
    , _ccCacheNodeType :: Maybe Text
      -- ^ The name of the compute and memory capacity node type for the
      -- cache cluster.
    , _ccEngine :: Maybe Text
      -- ^ The name of the cache engine (memcached or redis) to be used for
      -- this cache cluster.
    , _ccEngineVersion :: Maybe Text
      -- ^ The version of the cache engine version that is used in this
      -- cache cluster.
    , _ccCacheClusterStatus :: Maybe Text
      -- ^ The current state of this cache cluster - creating, available,
      -- etc.
    , _ccNumCacheNodes :: Maybe Integer
      -- ^ The number of cache nodes in the cache cluster.
    , _ccPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the cache cluster is
      -- located or "Multiple" if the cache nodes are located in different
      -- Availability Zones.
    , _ccCacheClusterCreateTime :: Maybe ISO8601
      -- ^ The date and time when the cache cluster was created.
    , _ccPreferredMaintenanceWindow :: Maybe Text
      -- ^ The time range (in UTC) during which weekly system maintenance
      -- can occur.
    , _ccPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ A group of settings that will be applied to the cache cluster in
      -- the future, or that are currently being applied.
    , _ccNotificationConfiguration :: Maybe NotificationConfiguration
      -- ^ Describes a notification topic and its status. Notification
      -- topics are used for publishing ElastiCache events to subscribers
      -- using Amazon Simple Notification Service (SNS).
    , _ccCacheSecurityGroups :: [CacheSecurityGroupMembership]
      -- ^ A list of cache security group elements, composed of name and
      -- status sub-elements.
    , _ccCacheParameterGroup :: Maybe CacheParameterGroupStatus
      -- ^ The status of the cache parameter group.
    , _ccCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group associated with the cache
      -- cluster.
    , _ccCacheNodes :: [CacheNode]
      -- ^ A list of cache nodes that are members of the cache cluster.
    , _ccAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ If true, then minor version patches are applied automatically; if
      -- false, then automatic minor version patches are disabled.
    , _ccSecurityGroups :: [SecurityGroupMembership]
      -- ^ A list of VPC Security Groups associated with the cache cluster.
    , _ccReplicationGroupId :: Maybe Text
      -- ^ The replication group to which this cache cluster belongs. If
      -- this field is empty, the cache cluster is not associated with any
      -- replication group.
    , _ccSnapshotRetentionLimit :: Maybe Integer
      -- ^ The number of days for which ElastiCache will retain automatic
      -- cache cluster snapshots before deleting them. For example, if you
      -- set SnapshotRetentionLimit to 5, then a snapshot that was taken
      -- today will be retained for 5 days before being deleted.
      -- ImportantIf the value of SnapshotRetentionLimit is set to zero
      -- (0), backups are turned off.
    , _ccSnapshotWindow :: Maybe Text
      -- ^ The daily time range (in UTC) during which ElastiCache will begin
      -- taking a daily snapshot of your cache cluster. Example:
      -- 05:00-09:00.
    } deriving (Show, Generic)

-- | The user-supplied identifier of the cache cluster. This is a unique key
-- that identifies a cache cluster.
ccCacheClusterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccCacheClusterId f x =
    (\y -> x { _ccCacheClusterId = y })
       <$> f (_ccCacheClusterId x)
{-# INLINE ccCacheClusterId #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ccConfigurationEndpoint
    :: Functor f
    => (Maybe Endpoint
    -> f (Maybe Endpoint))
    -> CacheCluster
    -> f CacheCluster
ccConfigurationEndpoint f x =
    (\y -> x { _ccConfigurationEndpoint = y })
       <$> f (_ccConfigurationEndpoint x)
{-# INLINE ccConfigurationEndpoint #-}

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
ccClientDownloadLandingPage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccClientDownloadLandingPage f x =
    (\y -> x { _ccClientDownloadLandingPage = y })
       <$> f (_ccClientDownloadLandingPage x)
{-# INLINE ccClientDownloadLandingPage #-}

-- | The name of the compute and memory capacity node type for the cache
-- cluster.
ccCacheNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccCacheNodeType f x =
    (\y -> x { _ccCacheNodeType = y })
       <$> f (_ccCacheNodeType x)
{-# INLINE ccCacheNodeType #-}

-- | The name of the cache engine (memcached or redis) to be used for this cache
-- cluster.
ccEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccEngine f x =
    (\y -> x { _ccEngine = y })
       <$> f (_ccEngine x)
{-# INLINE ccEngine #-}

-- | The version of the cache engine version that is used in this cache cluster.
ccEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccEngineVersion f x =
    (\y -> x { _ccEngineVersion = y })
       <$> f (_ccEngineVersion x)
{-# INLINE ccEngineVersion #-}

-- | The current state of this cache cluster - creating, available, etc.
ccCacheClusterStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccCacheClusterStatus f x =
    (\y -> x { _ccCacheClusterStatus = y })
       <$> f (_ccCacheClusterStatus x)
{-# INLINE ccCacheClusterStatus #-}

-- | The number of cache nodes in the cache cluster.
ccNumCacheNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CacheCluster
    -> f CacheCluster
ccNumCacheNodes f x =
    (\y -> x { _ccNumCacheNodes = y })
       <$> f (_ccNumCacheNodes x)
{-# INLINE ccNumCacheNodes #-}

-- | The name of the Availability Zone in which the cache cluster is located or
-- "Multiple" if the cache nodes are located in different Availability Zones.
ccPreferredAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccPreferredAvailabilityZone f x =
    (\y -> x { _ccPreferredAvailabilityZone = y })
       <$> f (_ccPreferredAvailabilityZone x)
{-# INLINE ccPreferredAvailabilityZone #-}

-- | The date and time when the cache cluster was created.
ccCacheClusterCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> CacheCluster
    -> f CacheCluster
ccCacheClusterCreateTime f x =
    (\y -> x { _ccCacheClusterCreateTime = y })
       <$> f (_ccCacheClusterCreateTime x)
{-# INLINE ccCacheClusterCreateTime #-}

-- | The time range (in UTC) during which weekly system maintenance can occur.
ccPreferredMaintenanceWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccPreferredMaintenanceWindow f x =
    (\y -> x { _ccPreferredMaintenanceWindow = y })
       <$> f (_ccPreferredMaintenanceWindow x)
{-# INLINE ccPreferredMaintenanceWindow #-}

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
ccPendingModifiedValues
    :: Functor f
    => (Maybe PendingModifiedValues
    -> f (Maybe PendingModifiedValues))
    -> CacheCluster
    -> f CacheCluster
ccPendingModifiedValues f x =
    (\y -> x { _ccPendingModifiedValues = y })
       <$> f (_ccPendingModifiedValues x)
{-# INLINE ccPendingModifiedValues #-}

-- | Describes a notification topic and its status. Notification topics are used
-- for publishing ElastiCache events to subscribers using Amazon Simple
-- Notification Service (SNS).
ccNotificationConfiguration
    :: Functor f
    => (Maybe NotificationConfiguration
    -> f (Maybe NotificationConfiguration))
    -> CacheCluster
    -> f CacheCluster
ccNotificationConfiguration f x =
    (\y -> x { _ccNotificationConfiguration = y })
       <$> f (_ccNotificationConfiguration x)
{-# INLINE ccNotificationConfiguration #-}

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
ccCacheSecurityGroups
    :: Functor f
    => ([CacheSecurityGroupMembership]
    -> f ([CacheSecurityGroupMembership]))
    -> CacheCluster
    -> f CacheCluster
ccCacheSecurityGroups f x =
    (\y -> x { _ccCacheSecurityGroups = y })
       <$> f (_ccCacheSecurityGroups x)
{-# INLINE ccCacheSecurityGroups #-}

-- | The status of the cache parameter group.
ccCacheParameterGroup
    :: Functor f
    => (Maybe CacheParameterGroupStatus
    -> f (Maybe CacheParameterGroupStatus))
    -> CacheCluster
    -> f CacheCluster
ccCacheParameterGroup f x =
    (\y -> x { _ccCacheParameterGroup = y })
       <$> f (_ccCacheParameterGroup x)
{-# INLINE ccCacheParameterGroup #-}

-- | The name of the cache subnet group associated with the cache cluster.
ccCacheSubnetGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccCacheSubnetGroupName f x =
    (\y -> x { _ccCacheSubnetGroupName = y })
       <$> f (_ccCacheSubnetGroupName x)
{-# INLINE ccCacheSubnetGroupName #-}

-- | A list of cache nodes that are members of the cache cluster.
ccCacheNodes
    :: Functor f
    => ([CacheNode]
    -> f ([CacheNode]))
    -> CacheCluster
    -> f CacheCluster
ccCacheNodes f x =
    (\y -> x { _ccCacheNodes = y })
       <$> f (_ccCacheNodes x)
{-# INLINE ccCacheNodes #-}

-- | If true, then minor version patches are applied automatically; if false,
-- then automatic minor version patches are disabled.
ccAutoMinorVersionUpgrade
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CacheCluster
    -> f CacheCluster
ccAutoMinorVersionUpgrade f x =
    (\y -> x { _ccAutoMinorVersionUpgrade = y })
       <$> f (_ccAutoMinorVersionUpgrade x)
{-# INLINE ccAutoMinorVersionUpgrade #-}

-- | A list of VPC Security Groups associated with the cache cluster.
ccSecurityGroups
    :: Functor f
    => ([SecurityGroupMembership]
    -> f ([SecurityGroupMembership]))
    -> CacheCluster
    -> f CacheCluster
ccSecurityGroups f x =
    (\y -> x { _ccSecurityGroups = y })
       <$> f (_ccSecurityGroups x)
{-# INLINE ccSecurityGroups #-}

-- | The replication group to which this cache cluster belongs. If this field is
-- empty, the cache cluster is not associated with any replication group.
ccReplicationGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccReplicationGroupId f x =
    (\y -> x { _ccReplicationGroupId = y })
       <$> f (_ccReplicationGroupId x)
{-# INLINE ccReplicationGroupId #-}

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
ccSnapshotRetentionLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> CacheCluster
    -> f CacheCluster
ccSnapshotRetentionLimit f x =
    (\y -> x { _ccSnapshotRetentionLimit = y })
       <$> f (_ccSnapshotRetentionLimit x)
{-# INLINE ccSnapshotRetentionLimit #-}

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster. Example: 05:00-09:00.
ccSnapshotWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheCluster
    -> f CacheCluster
ccSnapshotWindow f x =
    (\y -> x { _ccSnapshotWindow = y })
       <$> f (_ccSnapshotWindow x)
{-# INLINE ccSnapshotWindow #-}

instance FromXML CacheCluster where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheCluster"

-- | Provides all of the details about a particular cache engine version.
data CacheEngineVersion = CacheEngineVersion
    { _cevEngine :: Maybe Text
      -- ^ The name of the cache engine.
    , _cevEngineVersion :: Maybe Text
      -- ^ The version number of the cache engine.
    , _cevCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of the cache parameter group family associated with this
      -- cache engine.
    , _cevCacheEngineDescription :: Maybe Text
      -- ^ The description of the cache engine.
    , _cevCacheEngineVersionDescription :: Maybe Text
      -- ^ The description of the cache engine version.
    } deriving (Show, Generic)

-- | The name of the cache engine.
cevEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheEngineVersion
    -> f CacheEngineVersion
cevEngine f x =
    (\y -> x { _cevEngine = y })
       <$> f (_cevEngine x)
{-# INLINE cevEngine #-}

-- | The version number of the cache engine.
cevEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheEngineVersion
    -> f CacheEngineVersion
cevEngineVersion f x =
    (\y -> x { _cevEngineVersion = y })
       <$> f (_cevEngineVersion x)
{-# INLINE cevEngineVersion #-}

-- | The name of the cache parameter group family associated with this cache
-- engine.
cevCacheParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheEngineVersion
    -> f CacheEngineVersion
cevCacheParameterGroupFamily f x =
    (\y -> x { _cevCacheParameterGroupFamily = y })
       <$> f (_cevCacheParameterGroupFamily x)
{-# INLINE cevCacheParameterGroupFamily #-}

-- | The description of the cache engine.
cevCacheEngineDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheEngineVersion
    -> f CacheEngineVersion
cevCacheEngineDescription f x =
    (\y -> x { _cevCacheEngineDescription = y })
       <$> f (_cevCacheEngineDescription x)
{-# INLINE cevCacheEngineDescription #-}

-- | The description of the cache engine version.
cevCacheEngineVersionDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheEngineVersion
    -> f CacheEngineVersion
cevCacheEngineVersionDescription f x =
    (\y -> x { _cevCacheEngineVersionDescription = y })
       <$> f (_cevCacheEngineVersionDescription x)
{-# INLINE cevCacheEngineVersionDescription #-}

instance FromXML CacheEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheEngineVersion"

-- | Represents an individual cache node within a cache cluster. Each cache node
-- runs its own instance of the cluster's protocol-compliant caching software
-- - either Memcached or Redis.
data CacheNode = CacheNode
    { _cnCacheNodeId :: Maybe Text
      -- ^ The cache node identifier. A node ID is a numeric identifier
      -- (0001, 0002, etc.). The combination of cluster ID and node ID
      -- uniquely identifies every cache node used in a customer's AWS
      -- account.
    , _cnCacheNodeStatus :: Maybe Text
      -- ^ The current state of this cache node.
    , _cnCacheNodeCreateTime :: Maybe ISO8601
      -- ^ The date and time when the cache node was created.
    , _cnEndpoint :: Maybe Endpoint
      -- ^ The hostname and IP address for connecting to this cache node.
    , _cnParameterGroupStatus :: Maybe Text
      -- ^ The status of the parameter group applied to this cache node.
    , _cnSourceCacheNodeId :: Maybe Text
      -- ^ The ID of the primary node to which this read replica node is
      -- synchronized. If this field is empty, then this node is not
      -- associated with a primary cache cluster.
    , _cnCustomerAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone where this node was created and now
      -- resides.
    } deriving (Show, Generic)

-- | The cache node identifier. A node ID is a numeric identifier (0001, 0002,
-- etc.). The combination of cluster ID and node ID uniquely identifies every
-- cache node used in a customer's AWS account.
cnCacheNodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNode
    -> f CacheNode
cnCacheNodeId f x =
    (\y -> x { _cnCacheNodeId = y })
       <$> f (_cnCacheNodeId x)
{-# INLINE cnCacheNodeId #-}

-- | The current state of this cache node.
cnCacheNodeStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNode
    -> f CacheNode
cnCacheNodeStatus f x =
    (\y -> x { _cnCacheNodeStatus = y })
       <$> f (_cnCacheNodeStatus x)
{-# INLINE cnCacheNodeStatus #-}

-- | The date and time when the cache node was created.
cnCacheNodeCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> CacheNode
    -> f CacheNode
cnCacheNodeCreateTime f x =
    (\y -> x { _cnCacheNodeCreateTime = y })
       <$> f (_cnCacheNodeCreateTime x)
{-# INLINE cnCacheNodeCreateTime #-}

-- | The hostname and IP address for connecting to this cache node.
cnEndpoint
    :: Functor f
    => (Maybe Endpoint
    -> f (Maybe Endpoint))
    -> CacheNode
    -> f CacheNode
cnEndpoint f x =
    (\y -> x { _cnEndpoint = y })
       <$> f (_cnEndpoint x)
{-# INLINE cnEndpoint #-}

-- | The status of the parameter group applied to this cache node.
cnParameterGroupStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNode
    -> f CacheNode
cnParameterGroupStatus f x =
    (\y -> x { _cnParameterGroupStatus = y })
       <$> f (_cnParameterGroupStatus x)
{-# INLINE cnParameterGroupStatus #-}

-- | The ID of the primary node to which this read replica node is synchronized.
-- If this field is empty, then this node is not associated with a primary
-- cache cluster.
cnSourceCacheNodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNode
    -> f CacheNode
cnSourceCacheNodeId f x =
    (\y -> x { _cnSourceCacheNodeId = y })
       <$> f (_cnSourceCacheNodeId x)
{-# INLINE cnSourceCacheNodeId #-}

-- | The Availability Zone where this node was created and now resides.
cnCustomerAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNode
    -> f CacheNode
cnCustomerAvailabilityZone f x =
    (\y -> x { _cnCustomerAvailabilityZone = y })
       <$> f (_cnCustomerAvailabilityZone x)
{-# INLINE cnCustomerAvailabilityZone #-}

instance FromXML CacheNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNode"

instance ToQuery CacheNode where
    toQuery = genericQuery def

-- | A parameter that has a different value for each cache node type it is
-- applied to. For example, in a Redis cache cluster, a cache.m1.large cache
-- node type would have a larger maxmemory value than a cache.m1.small type.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter
    { _cntspParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , _cntspDescription :: Maybe Text
      -- ^ A description of the parameter.
    , _cntspSource :: Maybe Text
      -- ^ The source of the parameter value.
    , _cntspDataType :: Maybe Text
      -- ^ The valid data type for the parameter.
    , _cntspAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , _cntspIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be
      -- modified. Some parameters have security or operational
      -- implications that prevent them from being changed.
    , _cntspMinimumEngineVersion :: Maybe Text
      -- ^ The earliest cache engine version to which the parameter can
      -- apply.
    , _cntspCacheNodeTypeSpecificValues :: [CacheNodeTypeSpecificValue]
      -- ^ A list of cache node types and their corresponding values for
      -- this parameter.
    } deriving (Show, Generic)

-- | The name of the parameter.
cntspParameterName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspParameterName f x =
    (\y -> x { _cntspParameterName = y })
       <$> f (_cntspParameterName x)
{-# INLINE cntspParameterName #-}

-- | A description of the parameter.
cntspDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspDescription f x =
    (\y -> x { _cntspDescription = y })
       <$> f (_cntspDescription x)
{-# INLINE cntspDescription #-}

-- | The source of the parameter value.
cntspSource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspSource f x =
    (\y -> x { _cntspSource = y })
       <$> f (_cntspSource x)
{-# INLINE cntspSource #-}

-- | The valid data type for the parameter.
cntspDataType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspDataType f x =
    (\y -> x { _cntspDataType = y })
       <$> f (_cntspDataType x)
{-# INLINE cntspDataType #-}

-- | The valid range of values for the parameter.
cntspAllowedValues
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspAllowedValues f x =
    (\y -> x { _cntspAllowedValues = y })
       <$> f (_cntspAllowedValues x)
{-# INLINE cntspAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
cntspIsModifiable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspIsModifiable f x =
    (\y -> x { _cntspIsModifiable = y })
       <$> f (_cntspIsModifiable x)
{-# INLINE cntspIsModifiable #-}

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspMinimumEngineVersion f x =
    (\y -> x { _cntspMinimumEngineVersion = y })
       <$> f (_cntspMinimumEngineVersion x)
{-# INLINE cntspMinimumEngineVersion #-}

-- | A list of cache node types and their corresponding values for this
-- parameter.
cntspCacheNodeTypeSpecificValues
    :: Functor f
    => ([CacheNodeTypeSpecificValue]
    -> f ([CacheNodeTypeSpecificValue]))
    -> CacheNodeTypeSpecificParameter
    -> f CacheNodeTypeSpecificParameter
cntspCacheNodeTypeSpecificValues f x =
    (\y -> x { _cntspCacheNodeTypeSpecificValues = y })
       <$> f (_cntspCacheNodeTypeSpecificValues x)
{-# INLINE cntspCacheNodeTypeSpecificValues #-}

instance FromXML CacheNodeTypeSpecificParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNodeTypeSpecificParameter"

-- | A value that applies only to a certain cache node type.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { _cntsvCacheNodeType :: Maybe Text
      -- ^ The cache node type for which this value applies.
    , _cntsvValue :: Maybe Text
      -- ^ The value for the cache node type.
    } deriving (Show, Generic)

-- | The cache node type for which this value applies.
cntsvCacheNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificValue
    -> f CacheNodeTypeSpecificValue
cntsvCacheNodeType f x =
    (\y -> x { _cntsvCacheNodeType = y })
       <$> f (_cntsvCacheNodeType x)
{-# INLINE cntsvCacheNodeType #-}

-- | The value for the cache node type.
cntsvValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheNodeTypeSpecificValue
    -> f CacheNodeTypeSpecificValue
cntsvValue f x =
    (\y -> x { _cntsvValue = y })
       <$> f (_cntsvValue x)
{-# INLINE cntsvValue #-}

instance FromXML CacheNodeTypeSpecificValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNodeTypeSpecificValue"

instance ToQuery CacheNodeTypeSpecificValue where
    toQuery = genericQuery def

-- | Represents the output of a CreateCacheParameterGroup operation.
data CacheParameterGroup = CacheParameterGroup
    { _cpgCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    , _cpgCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of the cache parameter group family that this cache
      -- parameter group is compatible with.
    , _cpgDescription :: Maybe Text
      -- ^ The description for this cache parameter group.
    } deriving (Show, Generic)

-- | The name of the cache parameter group.
cpgCacheParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheParameterGroup
    -> f CacheParameterGroup
cpgCacheParameterGroupName f x =
    (\y -> x { _cpgCacheParameterGroupName = y })
       <$> f (_cpgCacheParameterGroupName x)
{-# INLINE cpgCacheParameterGroupName #-}

-- | The name of the cache parameter group family that this cache parameter
-- group is compatible with.
cpgCacheParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheParameterGroup
    -> f CacheParameterGroup
cpgCacheParameterGroupFamily f x =
    (\y -> x { _cpgCacheParameterGroupFamily = y })
       <$> f (_cpgCacheParameterGroupFamily x)
{-# INLINE cpgCacheParameterGroupFamily #-}

-- | The description for this cache parameter group.
cpgDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheParameterGroup
    -> f CacheParameterGroup
cpgDescription f x =
    (\y -> x { _cpgDescription = y })
       <$> f (_cpgDescription x)
{-# INLINE cpgDescription #-}

instance FromXML CacheParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroup"

-- | The status of the cache parameter group.
data CacheParameterGroupStatus = CacheParameterGroupStatus
    { _cpgsCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    , _cpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    , _cpgsCacheNodeIdsToReboot :: [Text]
      -- ^ A list of the cache node IDs which need to be rebooted for
      -- parameter changes to be applied. A node ID is a numeric
      -- identifier (0001, 0002, etc.).
    } deriving (Show, Generic)

-- | The name of the cache parameter group.
cpgsCacheParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheParameterGroupStatus
    -> f CacheParameterGroupStatus
cpgsCacheParameterGroupName f x =
    (\y -> x { _cpgsCacheParameterGroupName = y })
       <$> f (_cpgsCacheParameterGroupName x)
{-# INLINE cpgsCacheParameterGroupName #-}

-- | The status of parameter updates.
cpgsParameterApplyStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheParameterGroupStatus
    -> f CacheParameterGroupStatus
cpgsParameterApplyStatus f x =
    (\y -> x { _cpgsParameterApplyStatus = y })
       <$> f (_cpgsParameterApplyStatus x)
{-# INLINE cpgsParameterApplyStatus #-}

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cpgsCacheNodeIdsToReboot
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> CacheParameterGroupStatus
    -> f CacheParameterGroupStatus
cpgsCacheNodeIdsToReboot f x =
    (\y -> x { _cpgsCacheNodeIdsToReboot = y })
       <$> f (_cpgsCacheNodeIdsToReboot x)
{-# INLINE cpgsCacheNodeIdsToReboot #-}

instance FromXML CacheParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroupStatus"

instance ToQuery CacheParameterGroupStatus where
    toQuery = genericQuery def

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
data CacheSecurityGroup = CacheSecurityGroup
    { _csgOwnerId :: Maybe Text
      -- ^ The AWS account ID of the cache security group owner.
    , _csgCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group.
    , _csgDescription :: Maybe Text
      -- ^ The description of the cache security group.
    , _csgEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ A list of Amazon EC2 security groups that are associated with
      -- this cache security group.
    } deriving (Show, Generic)

-- | The AWS account ID of the cache security group owner.
csgOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSecurityGroup
    -> f CacheSecurityGroup
csgOwnerId f x =
    (\y -> x { _csgOwnerId = y })
       <$> f (_csgOwnerId x)
{-# INLINE csgOwnerId #-}

-- | The name of the cache security group.
csgCacheSecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSecurityGroup
    -> f CacheSecurityGroup
csgCacheSecurityGroupName f x =
    (\y -> x { _csgCacheSecurityGroupName = y })
       <$> f (_csgCacheSecurityGroupName x)
{-# INLINE csgCacheSecurityGroupName #-}

-- | The description of the cache security group.
csgDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSecurityGroup
    -> f CacheSecurityGroup
csgDescription f x =
    (\y -> x { _csgDescription = y })
       <$> f (_csgDescription x)
{-# INLINE csgDescription #-}

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
csgEC2SecurityGroups
    :: Functor f
    => ([EC2SecurityGroup]
    -> f ([EC2SecurityGroup]))
    -> CacheSecurityGroup
    -> f CacheSecurityGroup
csgEC2SecurityGroups f x =
    (\y -> x { _csgEC2SecurityGroups = y })
       <$> f (_csgEC2SecurityGroups x)
{-# INLINE csgEC2SecurityGroups #-}

instance FromXML CacheSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroup"

-- | Represents a cache cluster's status within a particular cache security
-- group.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership
    { _csgmCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group.
    , _csgmStatus :: Maybe Text
      -- ^ The membership status in the cache security group. The status
      -- changes when a cache security group is modified, or when the
      -- cache security groups assigned to a cache cluster are modified.
    } deriving (Show, Generic)

-- | The name of the cache security group.
csgmCacheSecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSecurityGroupMembership
    -> f CacheSecurityGroupMembership
csgmCacheSecurityGroupName f x =
    (\y -> x { _csgmCacheSecurityGroupName = y })
       <$> f (_csgmCacheSecurityGroupName x)
{-# INLINE csgmCacheSecurityGroupName #-}

-- | The membership status in the cache security group. The status changes when
-- a cache security group is modified, or when the cache security groups
-- assigned to a cache cluster are modified.
csgmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSecurityGroupMembership
    -> f CacheSecurityGroupMembership
csgmStatus f x =
    (\y -> x { _csgmStatus = y })
       <$> f (_csgmStatus x)
{-# INLINE csgmStatus #-}

instance FromXML CacheSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroup"

instance ToQuery CacheSecurityGroupMembership where
    toQuery = genericQuery def

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
data CacheSubnetGroup = CacheSubnetGroup
    { _csiCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group.
    , _csiCacheSubnetGroupDescription :: Maybe Text
      -- ^ The description of the cache subnet group.
    , _csiVpcId :: Maybe Text
      -- ^ The Amazon Virtual Private Cloud identifier (VPC ID) of the cache
      -- subnet group.
    , _csiSubnets :: [Subnet]
      -- ^ A list of subnets associated with the cache subnet group.
    } deriving (Show, Generic)

-- | The name of the cache subnet group.
csiCacheSubnetGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSubnetGroup
    -> f CacheSubnetGroup
csiCacheSubnetGroupName f x =
    (\y -> x { _csiCacheSubnetGroupName = y })
       <$> f (_csiCacheSubnetGroupName x)
{-# INLINE csiCacheSubnetGroupName #-}

-- | The description of the cache subnet group.
csiCacheSubnetGroupDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSubnetGroup
    -> f CacheSubnetGroup
csiCacheSubnetGroupDescription f x =
    (\y -> x { _csiCacheSubnetGroupDescription = y })
       <$> f (_csiCacheSubnetGroupDescription x)
{-# INLINE csiCacheSubnetGroupDescription #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
csiVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> CacheSubnetGroup
    -> f CacheSubnetGroup
csiVpcId f x =
    (\y -> x { _csiVpcId = y })
       <$> f (_csiVpcId x)
{-# INLINE csiVpcId #-}

-- | A list of subnets associated with the cache subnet group.
csiSubnets
    :: Functor f
    => ([Subnet]
    -> f ([Subnet]))
    -> CacheSubnetGroup
    -> f CacheSubnetGroup
csiSubnets f x =
    (\y -> x { _csiSubnets = y })
       <$> f (_csiSubnets x)
{-# INLINE csiSubnets #-}

instance FromXML CacheSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSubnetGroup"

-- | Provides ownership and status information for an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
      -- ^ The status of the Amazon EC2 security group.
    , _ecsgEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the Amazon EC2 security group.
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS account ID of the Amazon EC2 security group owner.
    } deriving (Show, Generic)

-- | The status of the Amazon EC2 security group.
ecsgStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgStatus f x =
    (\y -> x { _ecsgStatus = y })
       <$> f (_ecsgStatus x)
{-# INLINE ecsgStatus #-}

-- | The name of the Amazon EC2 security group.
ecsgEC2SecurityGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgEC2SecurityGroupName f x =
    (\y -> x { _ecsgEC2SecurityGroupName = y })
       <$> f (_ecsgEC2SecurityGroupName x)
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | The AWS account ID of the Amazon EC2 security group owner.
ecsgEC2SecurityGroupOwnerId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EC2SecurityGroup
    -> f EC2SecurityGroup
ecsgEC2SecurityGroupOwnerId f x =
    (\y -> x { _ecsgEC2SecurityGroupOwnerId = y })
       <$> f (_ecsgEC2SecurityGroupOwnerId x)
{-# INLINE ecsgEC2SecurityGroupOwnerId #-}

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericQuery def

-- | Represents the information required for client programs to connect to a
-- cache node.
data Endpoint = Endpoint
    { _eAddress :: Maybe Text
      -- ^ The DNS hostname of the cache node.
    , _ePort :: Maybe Integer
      -- ^ The port number that the cache engine is listening on.
    } deriving (Show, Generic)

-- | The DNS hostname of the cache node.
eAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Endpoint
    -> f Endpoint
eAddress f x =
    (\y -> x { _eAddress = y })
       <$> f (_eAddress x)
{-# INLINE eAddress #-}

-- | The port number that the cache engine is listening on.
ePort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Endpoint
    -> f Endpoint
ePort f x =
    (\y -> x { _ePort = y })
       <$> f (_ePort x)
{-# INLINE ePort #-}

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericQuery def

-- | Represents the output of a DescribeEngineDefaultParameters operation.
data EngineDefaults = EngineDefaults
    { _edCacheParameterGroupFamily :: Maybe Text
      -- ^ Specifies the name of the cache parameter group family to which
      -- the engine default parameters apply.
    , _edMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _edParameters :: [Parameter]
      -- ^ Contains a list of engine default parameters.
    , _edCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
      -- ^ A list of parameters specific to a particular cache node type.
      -- Each element in the list contains detailed information about one
      -- parameter.
    } deriving (Show, Generic)

-- | Specifies the name of the cache parameter group family to which the engine
-- default parameters apply.
edCacheParameterGroupFamily
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EngineDefaults
    -> f EngineDefaults
edCacheParameterGroupFamily f x =
    (\y -> x { _edCacheParameterGroupFamily = y })
       <$> f (_edCacheParameterGroupFamily x)
{-# INLINE edCacheParameterGroupFamily #-}

-- | Provides an identifier to allow retrieval of paginated results.
edMarker
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> EngineDefaults
    -> f EngineDefaults
edMarker f x =
    (\y -> x { _edMarker = y })
       <$> f (_edMarker x)
{-# INLINE edMarker #-}

-- | Contains a list of engine default parameters.
edParameters
    :: Functor f
    => ([Parameter]
    -> f ([Parameter]))
    -> EngineDefaults
    -> f EngineDefaults
edParameters f x =
    (\y -> x { _edParameters = y })
       <$> f (_edParameters x)
{-# INLINE edParameters #-}

-- | A list of parameters specific to a particular cache node type. Each element
-- in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters
    :: Functor f
    => ([CacheNodeTypeSpecificParameter]
    -> f ([CacheNodeTypeSpecificParameter]))
    -> EngineDefaults
    -> f EngineDefaults
edCacheNodeTypeSpecificParameters f x =
    (\y -> x { _edCacheNodeTypeSpecificParameters = y })
       <$> f (_edCacheNodeTypeSpecificParameters x)
{-# INLINE edCacheNodeTypeSpecificParameters #-}

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

-- | Represents a single occurrence of something interesting within the system.
-- Some examples of events are creating a cache cluster, adding or removing a
-- cache node, or rebooting a node.
data Event = Event
    { _exSourceIdentifier :: Maybe Text
      -- ^ The identifier for the source of the event. For example, if the
      -- event occurred at the cache cluster level, the identifier would
      -- be the name of the cache cluster.
    , _exSourceType :: Maybe SourceType
      -- ^ Specifies the origin of this event - a cache cluster, a parameter
      -- group, a security group, etc.
    , _exMessage :: Maybe Text
      -- ^ The text of the event.
    , _exDate :: Maybe ISO8601
      -- ^ The date and time when the event occurred.
    } deriving (Show, Generic)

-- | The identifier for the source of the event. For example, if the event
-- occurred at the cache cluster level, the identifier would be the name of
-- the cache cluster.
exSourceIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
exSourceIdentifier f x =
    (\y -> x { _exSourceIdentifier = y })
       <$> f (_exSourceIdentifier x)
{-# INLINE exSourceIdentifier #-}

-- | Specifies the origin of this event - a cache cluster, a parameter group, a
-- security group, etc.
exSourceType
    :: Functor f
    => (Maybe SourceType
    -> f (Maybe SourceType))
    -> Event
    -> f Event
exSourceType f x =
    (\y -> x { _exSourceType = y })
       <$> f (_exSourceType x)
{-# INLINE exSourceType #-}

-- | The text of the event.
exMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Event
    -> f Event
exMessage f x =
    (\y -> x { _exMessage = y })
       <$> f (_exMessage x)
{-# INLINE exMessage #-}

-- | The date and time when the event occurred.
exDate
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Event
    -> f Event
exDate f x =
    (\y -> x { _exDate = y })
       <$> f (_exDate x)
{-# INLINE exDate #-}

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | Represents a collection of cache nodes in a replication group.
data NodeGroup = NodeGroup
    { _ngNodeGroupId :: Maybe Text
      -- ^ The identifier for the node group. A replication group contains
      -- only one node group; therefore, the node group ID is 0001.
    , _ngStatus :: Maybe Text
      -- ^ The current state of this replication group - creating,
      -- available, etc.
    , _ngPrimaryEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to
      -- connect to a cache node.
    , _ngNodeGroupMembers :: [NodeGroupMember]
      -- ^ A list containing information about individual nodes within the
      -- node group.
    } deriving (Show, Generic)

-- | The identifier for the node group. A replication group contains only one
-- node group; therefore, the node group ID is 0001.
ngNodeGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeGroup
    -> f NodeGroup
ngNodeGroupId f x =
    (\y -> x { _ngNodeGroupId = y })
       <$> f (_ngNodeGroupId x)
{-# INLINE ngNodeGroupId #-}

-- | The current state of this replication group - creating, available, etc.
ngStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeGroup
    -> f NodeGroup
ngStatus f x =
    (\y -> x { _ngStatus = y })
       <$> f (_ngStatus x)
{-# INLINE ngStatus #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ngPrimaryEndpoint
    :: Functor f
    => (Maybe Endpoint
    -> f (Maybe Endpoint))
    -> NodeGroup
    -> f NodeGroup
ngPrimaryEndpoint f x =
    (\y -> x { _ngPrimaryEndpoint = y })
       <$> f (_ngPrimaryEndpoint x)
{-# INLINE ngPrimaryEndpoint #-}

-- | A list containing information about individual nodes within the node group.
ngNodeGroupMembers
    :: Functor f
    => ([NodeGroupMember]
    -> f ([NodeGroupMember]))
    -> NodeGroup
    -> f NodeGroup
ngNodeGroupMembers f x =
    (\y -> x { _ngNodeGroupMembers = y })
       <$> f (_ngNodeGroupMembers x)
{-# INLINE ngNodeGroupMembers #-}

instance FromXML NodeGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroup"

instance ToQuery NodeGroup where
    toQuery = genericQuery def

-- | Represents a single node within a node group.
data NodeGroupMember = NodeGroupMember
    { _ngmCacheClusterId :: Maybe Text
      -- ^ The ID of the cache cluster to which the node belongs.
    , _ngmCacheNodeId :: Maybe Text
      -- ^ The ID of the node within its cache cluster. A node ID is a
      -- numeric identifier (0001, 0002, etc.).
    , _ngmReadEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to
      -- connect to a cache node.
    , _ngmPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the node is located.
    , _ngmCurrentRole :: Maybe Text
      -- ^ The role that is currently assigned to the node - primary or
      -- replica.
    } deriving (Show, Generic)

-- | The ID of the cache cluster to which the node belongs.
ngmCacheClusterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeGroupMember
    -> f NodeGroupMember
ngmCacheClusterId f x =
    (\y -> x { _ngmCacheClusterId = y })
       <$> f (_ngmCacheClusterId x)
{-# INLINE ngmCacheClusterId #-}

-- | The ID of the node within its cache cluster. A node ID is a numeric
-- identifier (0001, 0002, etc.).
ngmCacheNodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeGroupMember
    -> f NodeGroupMember
ngmCacheNodeId f x =
    (\y -> x { _ngmCacheNodeId = y })
       <$> f (_ngmCacheNodeId x)
{-# INLINE ngmCacheNodeId #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ngmReadEndpoint
    :: Functor f
    => (Maybe Endpoint
    -> f (Maybe Endpoint))
    -> NodeGroupMember
    -> f NodeGroupMember
ngmReadEndpoint f x =
    (\y -> x { _ngmReadEndpoint = y })
       <$> f (_ngmReadEndpoint x)
{-# INLINE ngmReadEndpoint #-}

-- | The name of the Availability Zone in which the node is located.
ngmPreferredAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeGroupMember
    -> f NodeGroupMember
ngmPreferredAvailabilityZone f x =
    (\y -> x { _ngmPreferredAvailabilityZone = y })
       <$> f (_ngmPreferredAvailabilityZone x)
{-# INLINE ngmPreferredAvailabilityZone #-}

-- | The role that is currently assigned to the node - primary or replica.
ngmCurrentRole
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeGroupMember
    -> f NodeGroupMember
ngmCurrentRole f x =
    (\y -> x { _ngmCurrentRole = y })
       <$> f (_ngmCurrentRole x)
{-# INLINE ngmCurrentRole #-}

instance FromXML NodeGroupMember where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroupMember"

instance ToQuery NodeGroupMember where
    toQuery = genericQuery def

-- | Represents an individual cache node in a snapshot of a cache cluster.
data NodeSnapshot = NodeSnapshot
    { _nsCacheNodeId :: Maybe Text
      -- ^ The cache node identifier for the node in the source cache
      -- cluster.
    , _nsCacheSize :: Maybe Text
      -- ^ The size of the cache on the source cache node.
    , _nsCacheNodeCreateTime :: Maybe ISO8601
      -- ^ The date and time when the cache node was created in the source
      -- cache cluster.
    , _nsSnapshotCreateTime :: Maybe ISO8601
      -- ^ The date and time when the source node's metadata and cache data
      -- set was obtained for the snapshot.
    } deriving (Show, Generic)

-- | The cache node identifier for the node in the source cache cluster.
nsCacheNodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeSnapshot
    -> f NodeSnapshot
nsCacheNodeId f x =
    (\y -> x { _nsCacheNodeId = y })
       <$> f (_nsCacheNodeId x)
{-# INLINE nsCacheNodeId #-}

-- | The size of the cache on the source cache node.
nsCacheSize
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NodeSnapshot
    -> f NodeSnapshot
nsCacheSize f x =
    (\y -> x { _nsCacheSize = y })
       <$> f (_nsCacheSize x)
{-# INLINE nsCacheSize #-}

-- | The date and time when the cache node was created in the source cache
-- cluster.
nsCacheNodeCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> NodeSnapshot
    -> f NodeSnapshot
nsCacheNodeCreateTime f x =
    (\y -> x { _nsCacheNodeCreateTime = y })
       <$> f (_nsCacheNodeCreateTime x)
{-# INLINE nsCacheNodeCreateTime #-}

-- | The date and time when the source node's metadata and cache data set was
-- obtained for the snapshot.
nsSnapshotCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> NodeSnapshot
    -> f NodeSnapshot
nsSnapshotCreateTime f x =
    (\y -> x { _nsSnapshotCreateTime = y })
       <$> f (_nsSnapshotCreateTime x)
{-# INLINE nsSnapshotCreateTime #-}

instance FromXML NodeSnapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeSnapshot"

instance ToQuery NodeSnapshot where
    toQuery = genericQuery def

-- | Describes a notification topic and its status. Notification topics are used
-- for publishing ElastiCache events to subscribers using Amazon Simple
-- Notification Service (SNS).
data NotificationConfiguration = NotificationConfiguration
    { _ncTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) that identifies the topic.
    , _ncTopicStatus :: Maybe Text
      -- ^ The current state of the topic.
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) that identifies the topic.
ncTopicArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NotificationConfiguration
    -> f NotificationConfiguration
ncTopicArn f x =
    (\y -> x { _ncTopicArn = y })
       <$> f (_ncTopicArn x)
{-# INLINE ncTopicArn #-}

-- | The current state of the topic.
ncTopicStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> NotificationConfiguration
    -> f NotificationConfiguration
ncTopicStatus f x =
    (\y -> x { _ncTopicStatus = y })
       <$> f (_ncTopicStatus x)
{-# INLINE ncTopicStatus #-}

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NotificationConfiguration"

instance ToQuery NotificationConfiguration where
    toQuery = genericQuery def

-- | Describes an individual setting that controls some aspect of ElastiCache
-- behavior.
data Parameter = Parameter
    { _prParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , _prParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    , _prDescription :: Maybe Text
      -- ^ A description of the parameter.
    , _prSource :: Maybe Text
      -- ^ The source of the parameter.
    , _prDataType :: Maybe Text
      -- ^ The valid data type for the parameter.
    , _prAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , _prIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be
      -- modified. Some parameters have security or operational
      -- implications that prevent them from being changed.
    , _prMinimumEngineVersion :: Maybe Text
      -- ^ The earliest cache engine version to which the parameter can
      -- apply.
    } deriving (Show, Generic)

-- | The name of the parameter.
prParameterName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prParameterName f x =
    (\y -> x { _prParameterName = y })
       <$> f (_prParameterName x)
{-# INLINE prParameterName #-}

-- | The value of the parameter.
prParameterValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prParameterValue f x =
    (\y -> x { _prParameterValue = y })
       <$> f (_prParameterValue x)
{-# INLINE prParameterValue #-}

-- | A description of the parameter.
prDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prDescription f x =
    (\y -> x { _prDescription = y })
       <$> f (_prDescription x)
{-# INLINE prDescription #-}

-- | The source of the parameter.
prSource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prSource f x =
    (\y -> x { _prSource = y })
       <$> f (_prSource x)
{-# INLINE prSource #-}

-- | The valid data type for the parameter.
prDataType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prDataType f x =
    (\y -> x { _prDataType = y })
       <$> f (_prDataType x)
{-# INLINE prDataType #-}

-- | The valid range of values for the parameter.
prAllowedValues
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prAllowedValues f x =
    (\y -> x { _prAllowedValues = y })
       <$> f (_prAllowedValues x)
{-# INLINE prAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
prIsModifiable
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Parameter
    -> f Parameter
prIsModifiable f x =
    (\y -> x { _prIsModifiable = y })
       <$> f (_prIsModifiable x)
{-# INLINE prIsModifiable #-}

-- | The earliest cache engine version to which the parameter can apply.
prMinimumEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Parameter
    -> f Parameter
prMinimumEngineVersion f x =
    (\y -> x { _prMinimumEngineVersion = y })
       <$> f (_prMinimumEngineVersion x)
{-# INLINE prMinimumEngineVersion #-}

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

-- | Describes a name-value pair that is used to update the value of a
-- parameter.
data ParameterNameValue = ParameterNameValue
    { _pnvParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , _pnvParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    } deriving (Show, Generic)

-- | The name of the parameter.
pnvParameterName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ParameterNameValue
    -> f ParameterNameValue
pnvParameterName f x =
    (\y -> x { _pnvParameterName = y })
       <$> f (_pnvParameterName x)
{-# INLINE pnvParameterName #-}

-- | The value of the parameter.
pnvParameterValue
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ParameterNameValue
    -> f ParameterNameValue
pnvParameterValue f x =
    (\y -> x { _pnvParameterValue = y })
       <$> f (_pnvParameterValue x)
{-# INLINE pnvParameterValue #-}

instance ToQuery ParameterNameValue where
    toQuery = genericQuery def

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
data PendingModifiedValues = PendingModifiedValues
    { _pmvNumCacheNodes :: Maybe Integer
      -- ^ The new number of cache nodes for the cache cluster.
    , _pmvCacheNodeIdsToRemove :: [Text]
      -- ^ A list of cache node IDs that are being removed (or will be
      -- removed) from the cache cluster. A node ID is a numeric
      -- identifier (0001, 0002, etc.).
    , _pmvEngineVersion :: Maybe Text
      -- ^ The new cache engine version that the cache cluster will run.
    } deriving (Show, Generic)

-- | The new number of cache nodes for the cache cluster.
pmvNumCacheNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvNumCacheNodes f x =
    (\y -> x { _pmvNumCacheNodes = y })
       <$> f (_pmvNumCacheNodes x)
{-# INLINE pmvNumCacheNodes #-}

-- | A list of cache node IDs that are being removed (or will be removed) from
-- the cache cluster. A node ID is a numeric identifier (0001, 0002, etc.).
pmvCacheNodeIdsToRemove
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvCacheNodeIdsToRemove f x =
    (\y -> x { _pmvCacheNodeIdsToRemove = y })
       <$> f (_pmvCacheNodeIdsToRemove x)
{-# INLINE pmvCacheNodeIdsToRemove #-}

-- | The new cache engine version that the cache cluster will run.
pmvEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> PendingModifiedValues
    -> f PendingModifiedValues
pmvEngineVersion f x =
    (\y -> x { _pmvEngineVersion = y })
       <$> f (_pmvEngineVersion x)
{-# INLINE pmvEngineVersion #-}

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericQuery def

-- | Contains the specific price and frequency of a recurring charges for a
-- reserved cache node, or for a reserved cache node offering.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount :: Maybe Double
      -- ^ The monetary amount of the recurring charge.
    , _rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency of the recurring charge.
    } deriving (Show, Generic)

-- | The monetary amount of the recurring charge.
rcRecurringChargeAmount
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> RecurringCharge
    -> f RecurringCharge
rcRecurringChargeAmount f x =
    (\y -> x { _rcRecurringChargeAmount = y })
       <$> f (_rcRecurringChargeAmount x)
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> RecurringCharge
    -> f RecurringCharge
rcRecurringChargeFrequency f x =
    (\y -> x { _rcRecurringChargeFrequency = y })
       <$> f (_rcRecurringChargeFrequency x)
{-# INLINE rcRecurringChargeFrequency #-}

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Contains all of the attributes of a specific replication group.
data ReplicationGroup = ReplicationGroup
    { _rgReplicationGroupId :: Maybe Text
      -- ^ The identifier for the replication group.
    , _rgDescription :: Maybe Text
      -- ^ The description of the replication group.
    , _rgStatus :: Maybe Text
      -- ^ The current state of this replication group - creating,
      -- available, etc.
    , _rgPendingModifiedValues :: Maybe ReplicationGroupPendingModifiedValues
      -- ^ A group of settings to be applied to the replication group,
      -- either immediately or during the next maintenance window.
    , _rgMemberClusters :: [Text]
      -- ^ The names of all the cache clusters that are part of this
      -- replication group.
    , _rgNodeGroups :: [NodeGroup]
      -- ^ A single element list with information about the nodes in the
      -- replication group.
    , _rgSnapshottingClusterId :: Maybe Text
      -- ^ The cache cluster ID that is used as the daily snapshot source
      -- for the replication group.
    } deriving (Show, Generic)

-- | The identifier for the replication group.
rgReplicationGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReplicationGroup
    -> f ReplicationGroup
rgReplicationGroupId f x =
    (\y -> x { _rgReplicationGroupId = y })
       <$> f (_rgReplicationGroupId x)
{-# INLINE rgReplicationGroupId #-}

-- | The description of the replication group.
rgDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReplicationGroup
    -> f ReplicationGroup
rgDescription f x =
    (\y -> x { _rgDescription = y })
       <$> f (_rgDescription x)
{-# INLINE rgDescription #-}

-- | The current state of this replication group - creating, available, etc.
rgStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReplicationGroup
    -> f ReplicationGroup
rgStatus f x =
    (\y -> x { _rgStatus = y })
       <$> f (_rgStatus x)
{-# INLINE rgStatus #-}

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
rgPendingModifiedValues
    :: Functor f
    => (Maybe ReplicationGroupPendingModifiedValues
    -> f (Maybe ReplicationGroupPendingModifiedValues))
    -> ReplicationGroup
    -> f ReplicationGroup
rgPendingModifiedValues f x =
    (\y -> x { _rgPendingModifiedValues = y })
       <$> f (_rgPendingModifiedValues x)
{-# INLINE rgPendingModifiedValues #-}

-- | The names of all the cache clusters that are part of this replication
-- group.
rgMemberClusters
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ReplicationGroup
    -> f ReplicationGroup
rgMemberClusters f x =
    (\y -> x { _rgMemberClusters = y })
       <$> f (_rgMemberClusters x)
{-# INLINE rgMemberClusters #-}

-- | A single element list with information about the nodes in the replication
-- group.
rgNodeGroups
    :: Functor f
    => ([NodeGroup]
    -> f ([NodeGroup]))
    -> ReplicationGroup
    -> f ReplicationGroup
rgNodeGroups f x =
    (\y -> x { _rgNodeGroups = y })
       <$> f (_rgNodeGroups x)
{-# INLINE rgNodeGroups #-}

-- | The cache cluster ID that is used as the daily snapshot source for the
-- replication group.
rgSnapshottingClusterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReplicationGroup
    -> f ReplicationGroup
rgSnapshottingClusterId f x =
    (\y -> x { _rgSnapshottingClusterId = y })
       <$> f (_rgSnapshottingClusterId x)
{-# INLINE rgSnapshottingClusterId #-}

instance FromXML ReplicationGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroup"

-- | Represents the output of a PurchaseReservedCacheNodesOffering operation.
data ReservedCacheNode = ReservedCacheNode
    { _rcnReservedCacheNodeId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , _rcnReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    , _rcnCacheNodeType :: Maybe Text
      -- ^ The cache node type for the reserved cache nodes.
    , _rcnStartTime :: Maybe ISO8601
      -- ^ The time the reservation started.
    , _rcnDuration :: Maybe Integer
      -- ^ The duration of the reservation in seconds.
    , _rcnFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this reserved cache node.
    , _rcnUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this reserved cache node.
    , _rcnCacheNodeCount :: Maybe Integer
      -- ^ The number of cache nodes that have been reserved.
    , _rcnProductDescription :: Maybe Text
      -- ^ The description of the reserved cache node.
    , _rcnOfferingType :: Maybe Text
      -- ^ The offering type of this reserved cache node.
    , _rcnState :: Maybe Text
      -- ^ The state of the reserved cache node.
    , _rcnRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved cache node.
    } deriving (Show, Generic)

-- | The unique identifier for the reservation.
rcnReservedCacheNodeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnReservedCacheNodeId f x =
    (\y -> x { _rcnReservedCacheNodeId = y })
       <$> f (_rcnReservedCacheNodeId x)
{-# INLINE rcnReservedCacheNodeId #-}

-- | The offering identifier.
rcnReservedCacheNodesOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnReservedCacheNodesOfferingId f x =
    (\y -> x { _rcnReservedCacheNodesOfferingId = y })
       <$> f (_rcnReservedCacheNodesOfferingId x)
{-# INLINE rcnReservedCacheNodesOfferingId #-}

-- | The cache node type for the reserved cache nodes.
rcnCacheNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnCacheNodeType f x =
    (\y -> x { _rcnCacheNodeType = y })
       <$> f (_rcnCacheNodeType x)
{-# INLINE rcnCacheNodeType #-}

-- | The time the reservation started.
rcnStartTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnStartTime f x =
    (\y -> x { _rcnStartTime = y })
       <$> f (_rcnStartTime x)
{-# INLINE rcnStartTime #-}

-- | The duration of the reservation in seconds.
rcnDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnDuration f x =
    (\y -> x { _rcnDuration = y })
       <$> f (_rcnDuration x)
{-# INLINE rcnDuration #-}

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnFixedPrice f x =
    (\y -> x { _rcnFixedPrice = y })
       <$> f (_rcnFixedPrice x)
{-# INLINE rcnFixedPrice #-}

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnUsagePrice f x =
    (\y -> x { _rcnUsagePrice = y })
       <$> f (_rcnUsagePrice x)
{-# INLINE rcnUsagePrice #-}

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnCacheNodeCount f x =
    (\y -> x { _rcnCacheNodeCount = y })
       <$> f (_rcnCacheNodeCount x)
{-# INLINE rcnCacheNodeCount #-}

-- | The description of the reserved cache node.
rcnProductDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnProductDescription f x =
    (\y -> x { _rcnProductDescription = y })
       <$> f (_rcnProductDescription x)
{-# INLINE rcnProductDescription #-}

-- | The offering type of this reserved cache node.
rcnOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnOfferingType f x =
    (\y -> x { _rcnOfferingType = y })
       <$> f (_rcnOfferingType x)
{-# INLINE rcnOfferingType #-}

-- | The state of the reserved cache node.
rcnState
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnState f x =
    (\y -> x { _rcnState = y })
       <$> f (_rcnState x)
{-# INLINE rcnState #-}

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedCacheNode
    -> f ReservedCacheNode
rcnRecurringCharges f x =
    (\y -> x { _rcnRecurringCharges = y })
       <$> f (_rcnRecurringCharges x)
{-# INLINE rcnRecurringCharges #-}

instance FromXML ReservedCacheNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNode"

-- | Describes all of the attributes of a reserved cache node offering.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering
    { _rcnoReservedCacheNodesOfferingId :: Maybe Text
      -- ^ A unique identifier for the reserved cache node offering.
    , _rcnoCacheNodeType :: Maybe Text
      -- ^ The cache node type for the reserved cache node.
    , _rcnoDuration :: Maybe Integer
      -- ^ The duration of the offering. in seconds.
    , _rcnoFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this offering.
    , _rcnoUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this offering.
    , _rcnoProductDescription :: Maybe Text
      -- ^ The cache engine used by the offering.
    , _rcnoOfferingType :: Maybe Text
      -- ^ The offering type.
    , _rcnoRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved cache node.
    } deriving (Show, Generic)

-- | A unique identifier for the reserved cache node offering.
rcnoReservedCacheNodesOfferingId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoReservedCacheNodesOfferingId f x =
    (\y -> x { _rcnoReservedCacheNodesOfferingId = y })
       <$> f (_rcnoReservedCacheNodesOfferingId x)
{-# INLINE rcnoReservedCacheNodesOfferingId #-}

-- | The cache node type for the reserved cache node.
rcnoCacheNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoCacheNodeType f x =
    (\y -> x { _rcnoCacheNodeType = y })
       <$> f (_rcnoCacheNodeType x)
{-# INLINE rcnoCacheNodeType #-}

-- | The duration of the offering. in seconds.
rcnoDuration
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoDuration f x =
    (\y -> x { _rcnoDuration = y })
       <$> f (_rcnoDuration x)
{-# INLINE rcnoDuration #-}

-- | The fixed price charged for this offering.
rcnoFixedPrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoFixedPrice f x =
    (\y -> x { _rcnoFixedPrice = y })
       <$> f (_rcnoFixedPrice x)
{-# INLINE rcnoFixedPrice #-}

-- | The hourly price charged for this offering.
rcnoUsagePrice
    :: Functor f
    => (Maybe Double
    -> f (Maybe Double))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoUsagePrice f x =
    (\y -> x { _rcnoUsagePrice = y })
       <$> f (_rcnoUsagePrice x)
{-# INLINE rcnoUsagePrice #-}

-- | The cache engine used by the offering.
rcnoProductDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoProductDescription f x =
    (\y -> x { _rcnoProductDescription = y })
       <$> f (_rcnoProductDescription x)
{-# INLINE rcnoProductDescription #-}

-- | The offering type.
rcnoOfferingType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoOfferingType f x =
    (\y -> x { _rcnoOfferingType = y })
       <$> f (_rcnoOfferingType x)
{-# INLINE rcnoOfferingType #-}

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges
    :: Functor f
    => ([RecurringCharge]
    -> f ([RecurringCharge]))
    -> ReservedCacheNodesOffering
    -> f ReservedCacheNodesOffering
rcnoRecurringCharges f x =
    (\y -> x { _rcnoRecurringCharges = y })
       <$> f (_rcnoRecurringCharges x)
{-# INLINE rcnoRecurringCharges #-}

instance FromXML ReservedCacheNodesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNodesOffering"

-- | Represents a single cache security group and its status..
data SecurityGroupMembership = SecurityGroupMembership
    { _sgmSecurityGroupId :: Maybe Text
      -- ^ The identifier of the cache security group.
    , _sgmStatus :: Maybe Text
      -- ^ The status of the cache security group membership. The status
      -- changes whenever a cache security group is modified, or when the
      -- cache security groups assigned to a cache cluster are modified.
    } deriving (Show, Generic)

-- | The identifier of the cache security group.
sgmSecurityGroupId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SecurityGroupMembership
    -> f SecurityGroupMembership
sgmSecurityGroupId f x =
    (\y -> x { _sgmSecurityGroupId = y })
       <$> f (_sgmSecurityGroupId x)
{-# INLINE sgmSecurityGroupId #-}

-- | The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cache cluster are modified.
sgmStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SecurityGroupMembership
    -> f SecurityGroupMembership
sgmStatus f x =
    (\y -> x { _sgmStatus = y })
       <$> f (_sgmStatus x)
{-# INLINE sgmStatus #-}

instance FromXML SecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SecurityGroupMembership"

instance ToQuery SecurityGroupMembership where
    toQuery = genericQuery def

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
data Snapshot = Snapshot
    { _stSnapshotName :: Maybe Text
      -- ^ The name of a snapshot. For an automatic snapshot, the name is
      -- system-generated; for a manual snapshot, this is the
      -- user-provided name.
    , _stCacheClusterId :: Maybe Text
      -- ^ The user-supplied identifier of the source cache cluster.
    , _stSnapshotStatus :: Maybe Text
      -- ^ The status of the snapshot. Valid values: creating | available |
      -- restoring | copying | deleting.
    , _stSnapshotSource :: Maybe Text
      -- ^ Indicates whether the snapshot is from an automatic backup
      -- (automated) or was created manually (manual).
    , _stCacheNodeType :: Maybe Text
      -- ^ The name of the compute and memory capacity node type for the
      -- source cache cluster.
    , _stEngine :: Maybe Text
      -- ^ The name of the cache engine (memcached or redis) used by the
      -- source cache cluster.
    , _stEngineVersion :: Maybe Text
      -- ^ The version of the cache engine version that is used by the
      -- source cache cluster.
    , _stNumCacheNodes :: Maybe Integer
      -- ^ The number of cache nodes in the source cache cluster.
    , _stPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the source cache
      -- cluster is located.
    , _stCacheClusterCreateTime :: Maybe ISO8601
      -- ^ The date and time when the source cache cluster was created.
    , _stPreferredMaintenanceWindow :: Maybe Text
      -- ^ The time range (in UTC) during which weekly system maintenance
      -- can occur on the source cache cluster.
    , _stTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for the topic used by the source
      -- cache cluster for publishing notifications.
    , _stPort :: Maybe Integer
      -- ^ The port number used by each cache nodes in the source cache
      -- cluster.
    , _stCacheParameterGroupName :: Maybe Text
      -- ^ The cache parameter group that is associated with the source
      -- cache cluster.
    , _stCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group associated with the source
      -- cache cluster.
    , _stVpcId :: Maybe Text
      -- ^ The Amazon Virtual Private Cloud identifier (VPC ID) of the cache
      -- subnet group for the source cache cluster.
    , _stAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ For the source cache cluster, indicates whether minor version
      -- patches are applied automatically (true) or not (false).
    , _stSnapshotRetentionLimit :: Maybe Integer
      -- ^ For an automatic snapshot, the number of days for which
      -- ElastiCache will retain the snapshot before deleting it. For
      -- manual snapshots, this field reflects the SnapshotRetentionLimit
      -- for the source cache cluster when the snapshot was created. This
      -- field is otherwise ignored: Manual snapshots do not expire, and
      -- can only be deleted using the DeleteSnapshot action. ImportantIf
      -- the value of SnapshotRetentionLimit is set to zero (0), backups
      -- are turned off.
    , _stSnapshotWindow :: Maybe Text
      -- ^ The daily time range during which ElastiCache takes daily
      -- snapshots of the source cache cluster.
    , _stNodeSnapshots :: [NodeSnapshot]
      -- ^ A list of the cache nodes in the source cache cluster.
    } deriving (Show, Generic)

-- | The name of a snapshot. For an automatic snapshot, the name is
-- system-generated; for a manual snapshot, this is the user-provided name.
stSnapshotName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stSnapshotName f x =
    (\y -> x { _stSnapshotName = y })
       <$> f (_stSnapshotName x)
{-# INLINE stSnapshotName #-}

-- | The user-supplied identifier of the source cache cluster.
stCacheClusterId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stCacheClusterId f x =
    (\y -> x { _stCacheClusterId = y })
       <$> f (_stCacheClusterId x)
{-# INLINE stCacheClusterId #-}

-- | The status of the snapshot. Valid values: creating | available | restoring
-- | copying | deleting.
stSnapshotStatus
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stSnapshotStatus f x =
    (\y -> x { _stSnapshotStatus = y })
       <$> f (_stSnapshotStatus x)
{-# INLINE stSnapshotStatus #-}

-- | Indicates whether the snapshot is from an automatic backup (automated) or
-- was created manually (manual).
stSnapshotSource
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stSnapshotSource f x =
    (\y -> x { _stSnapshotSource = y })
       <$> f (_stSnapshotSource x)
{-# INLINE stSnapshotSource #-}

-- | The name of the compute and memory capacity node type for the source cache
-- cluster.
stCacheNodeType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stCacheNodeType f x =
    (\y -> x { _stCacheNodeType = y })
       <$> f (_stCacheNodeType x)
{-# INLINE stCacheNodeType #-}

-- | The name of the cache engine (memcached or redis) used by the source cache
-- cluster.
stEngine
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stEngine f x =
    (\y -> x { _stEngine = y })
       <$> f (_stEngine x)
{-# INLINE stEngine #-}

-- | The version of the cache engine version that is used by the source cache
-- cluster.
stEngineVersion
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stEngineVersion f x =
    (\y -> x { _stEngineVersion = y })
       <$> f (_stEngineVersion x)
{-# INLINE stEngineVersion #-}

-- | The number of cache nodes in the source cache cluster.
stNumCacheNodes
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
stNumCacheNodes f x =
    (\y -> x { _stNumCacheNodes = y })
       <$> f (_stNumCacheNodes x)
{-# INLINE stNumCacheNodes #-}

-- | The name of the Availability Zone in which the source cache cluster is
-- located.
stPreferredAvailabilityZone
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stPreferredAvailabilityZone f x =
    (\y -> x { _stPreferredAvailabilityZone = y })
       <$> f (_stPreferredAvailabilityZone x)
{-# INLINE stPreferredAvailabilityZone #-}

-- | The date and time when the source cache cluster was created.
stCacheClusterCreateTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> Snapshot
    -> f Snapshot
stCacheClusterCreateTime f x =
    (\y -> x { _stCacheClusterCreateTime = y })
       <$> f (_stCacheClusterCreateTime x)
{-# INLINE stCacheClusterCreateTime #-}

-- | The time range (in UTC) during which weekly system maintenance can occur on
-- the source cache cluster.
stPreferredMaintenanceWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stPreferredMaintenanceWindow f x =
    (\y -> x { _stPreferredMaintenanceWindow = y })
       <$> f (_stPreferredMaintenanceWindow x)
{-# INLINE stPreferredMaintenanceWindow #-}

-- | The Amazon Resource Name (ARN) for the topic used by the source cache
-- cluster for publishing notifications.
stTopicArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stTopicArn f x =
    (\y -> x { _stTopicArn = y })
       <$> f (_stTopicArn x)
{-# INLINE stTopicArn #-}

-- | The port number used by each cache nodes in the source cache cluster.
stPort
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
stPort f x =
    (\y -> x { _stPort = y })
       <$> f (_stPort x)
{-# INLINE stPort #-}

-- | The cache parameter group that is associated with the source cache cluster.
stCacheParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stCacheParameterGroupName f x =
    (\y -> x { _stCacheParameterGroupName = y })
       <$> f (_stCacheParameterGroupName x)
{-# INLINE stCacheParameterGroupName #-}

-- | The name of the cache subnet group associated with the source cache
-- cluster.
stCacheSubnetGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stCacheSubnetGroupName f x =
    (\y -> x { _stCacheSubnetGroupName = y })
       <$> f (_stCacheSubnetGroupName x)
{-# INLINE stCacheSubnetGroupName #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cache cluster.
stVpcId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stVpcId f x =
    (\y -> x { _stVpcId = y })
       <$> f (_stVpcId x)
{-# INLINE stVpcId #-}

-- | For the source cache cluster, indicates whether minor version patches are
-- applied automatically (true) or not (false).
stAutoMinorVersionUpgrade
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> Snapshot
    -> f Snapshot
stAutoMinorVersionUpgrade f x =
    (\y -> x { _stAutoMinorVersionUpgrade = y })
       <$> f (_stAutoMinorVersionUpgrade x)
{-# INLINE stAutoMinorVersionUpgrade #-}

-- | For an automatic snapshot, the number of days for which ElastiCache will
-- retain the snapshot before deleting it. For manual snapshots, this field
-- reflects the SnapshotRetentionLimit for the source cache cluster when the
-- snapshot was created. This field is otherwise ignored: Manual snapshots do
-- not expire, and can only be deleted using the DeleteSnapshot action.
-- ImportantIf the value of SnapshotRetentionLimit is set to zero (0), backups
-- are turned off.
stSnapshotRetentionLimit
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> Snapshot
    -> f Snapshot
stSnapshotRetentionLimit f x =
    (\y -> x { _stSnapshotRetentionLimit = y })
       <$> f (_stSnapshotRetentionLimit x)
{-# INLINE stSnapshotRetentionLimit #-}

-- | The daily time range during which ElastiCache takes daily snapshots of the
-- source cache cluster.
stSnapshotWindow
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Snapshot
    -> f Snapshot
stSnapshotWindow f x =
    (\y -> x { _stSnapshotWindow = y })
       <$> f (_stSnapshotWindow x)
{-# INLINE stSnapshotWindow #-}

-- | A list of the cache nodes in the source cache cluster.
stNodeSnapshots
    :: Functor f
    => ([NodeSnapshot]
    -> f ([NodeSnapshot]))
    -> Snapshot
    -> f Snapshot
stNodeSnapshots f x =
    (\y -> x { _stNodeSnapshots = y })
       <$> f (_stNodeSnapshots x)
{-# INLINE stNodeSnapshots #-}

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Snapshot"

-- | Represents the subnet associated with a cache cluster. This parameter
-- refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and
-- used with ElastiCache.
data Subnet = Subnet
    { _sssssuSubnetIdentifier :: Maybe Text
      -- ^ The unique identifier for the subnet.
    , _sssssuSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ The Availability Zone associated with the subnet.
    } deriving (Show, Generic)

-- | The unique identifier for the subnet.
sssssuSubnetIdentifier
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subnet
    -> f Subnet
sssssuSubnetIdentifier f x =
    (\y -> x { _sssssuSubnetIdentifier = y })
       <$> f (_sssssuSubnetIdentifier x)
{-# INLINE sssssuSubnetIdentifier #-}

-- | The Availability Zone associated with the subnet.
sssssuSubnetAvailabilityZone
    :: Functor f
    => (Maybe AvailabilityZone
    -> f (Maybe AvailabilityZone))
    -> Subnet
    -> f Subnet
sssssuSubnetAvailabilityZone f x =
    (\y -> x { _sssssuSubnetAvailabilityZone = y })
       <$> f (_sssssuSubnetAvailabilityZone x)
{-# INLINE sssssuSubnetAvailabilityZone #-}

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def
