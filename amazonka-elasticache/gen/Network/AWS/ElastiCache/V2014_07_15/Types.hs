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
azName :: Lens' AvailabilityZone (Maybe Text)
azName f x =
    f (_azName x)
        <&> \y -> x { _azName = y }
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
rgpmvPrimaryClusterId :: Lens' ReplicationGroupPendingModifiedValues (Maybe Text)
rgpmvPrimaryClusterId f x =
    f (_rgpmvPrimaryClusterId x)
        <&> \y -> x { _rgpmvPrimaryClusterId = y }
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
ccCacheClusterId :: Lens' CacheCluster (Maybe Text)
ccCacheClusterId f x =
    f (_ccCacheClusterId x)
        <&> \y -> x { _ccCacheClusterId = y }
{-# INLINE ccCacheClusterId #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ccConfigurationEndpoint :: Lens' CacheCluster (Maybe Endpoint)
ccConfigurationEndpoint f x =
    f (_ccConfigurationEndpoint x)
        <&> \y -> x { _ccConfigurationEndpoint = y }
{-# INLINE ccConfigurationEndpoint #-}

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
ccClientDownloadLandingPage :: Lens' CacheCluster (Maybe Text)
ccClientDownloadLandingPage f x =
    f (_ccClientDownloadLandingPage x)
        <&> \y -> x { _ccClientDownloadLandingPage = y }
{-# INLINE ccClientDownloadLandingPage #-}

-- | The name of the compute and memory capacity node type for the cache
-- cluster.
ccCacheNodeType :: Lens' CacheCluster (Maybe Text)
ccCacheNodeType f x =
    f (_ccCacheNodeType x)
        <&> \y -> x { _ccCacheNodeType = y }
{-# INLINE ccCacheNodeType #-}

-- | The name of the cache engine (memcached or redis) to be used for this cache
-- cluster.
ccEngine :: Lens' CacheCluster (Maybe Text)
ccEngine f x =
    f (_ccEngine x)
        <&> \y -> x { _ccEngine = y }
{-# INLINE ccEngine #-}

-- | The version of the cache engine version that is used in this cache cluster.
ccEngineVersion :: Lens' CacheCluster (Maybe Text)
ccEngineVersion f x =
    f (_ccEngineVersion x)
        <&> \y -> x { _ccEngineVersion = y }
{-# INLINE ccEngineVersion #-}

-- | The current state of this cache cluster - creating, available, etc.
ccCacheClusterStatus :: Lens' CacheCluster (Maybe Text)
ccCacheClusterStatus f x =
    f (_ccCacheClusterStatus x)
        <&> \y -> x { _ccCacheClusterStatus = y }
{-# INLINE ccCacheClusterStatus #-}

-- | The number of cache nodes in the cache cluster.
ccNumCacheNodes :: Lens' CacheCluster (Maybe Integer)
ccNumCacheNodes f x =
    f (_ccNumCacheNodes x)
        <&> \y -> x { _ccNumCacheNodes = y }
{-# INLINE ccNumCacheNodes #-}

-- | The name of the Availability Zone in which the cache cluster is located or
-- "Multiple" if the cache nodes are located in different Availability Zones.
ccPreferredAvailabilityZone :: Lens' CacheCluster (Maybe Text)
ccPreferredAvailabilityZone f x =
    f (_ccPreferredAvailabilityZone x)
        <&> \y -> x { _ccPreferredAvailabilityZone = y }
{-# INLINE ccPreferredAvailabilityZone #-}

-- | The date and time when the cache cluster was created.
ccCacheClusterCreateTime :: Lens' CacheCluster (Maybe ISO8601)
ccCacheClusterCreateTime f x =
    f (_ccCacheClusterCreateTime x)
        <&> \y -> x { _ccCacheClusterCreateTime = y }
{-# INLINE ccCacheClusterCreateTime #-}

-- | The time range (in UTC) during which weekly system maintenance can occur.
ccPreferredMaintenanceWindow :: Lens' CacheCluster (Maybe Text)
ccPreferredMaintenanceWindow f x =
    f (_ccPreferredMaintenanceWindow x)
        <&> \y -> x { _ccPreferredMaintenanceWindow = y }
{-# INLINE ccPreferredMaintenanceWindow #-}

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
ccPendingModifiedValues :: Lens' CacheCluster (Maybe PendingModifiedValues)
ccPendingModifiedValues f x =
    f (_ccPendingModifiedValues x)
        <&> \y -> x { _ccPendingModifiedValues = y }
{-# INLINE ccPendingModifiedValues #-}

-- | Describes a notification topic and its status. Notification topics are used
-- for publishing ElastiCache events to subscribers using Amazon Simple
-- Notification Service (SNS).
ccNotificationConfiguration :: Lens' CacheCluster (Maybe NotificationConfiguration)
ccNotificationConfiguration f x =
    f (_ccNotificationConfiguration x)
        <&> \y -> x { _ccNotificationConfiguration = y }
{-# INLINE ccNotificationConfiguration #-}

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
ccCacheSecurityGroups :: Lens' CacheCluster ([CacheSecurityGroupMembership])
ccCacheSecurityGroups f x =
    f (_ccCacheSecurityGroups x)
        <&> \y -> x { _ccCacheSecurityGroups = y }
{-# INLINE ccCacheSecurityGroups #-}

-- | The status of the cache parameter group.
ccCacheParameterGroup :: Lens' CacheCluster (Maybe CacheParameterGroupStatus)
ccCacheParameterGroup f x =
    f (_ccCacheParameterGroup x)
        <&> \y -> x { _ccCacheParameterGroup = y }
{-# INLINE ccCacheParameterGroup #-}

-- | The name of the cache subnet group associated with the cache cluster.
ccCacheSubnetGroupName :: Lens' CacheCluster (Maybe Text)
ccCacheSubnetGroupName f x =
    f (_ccCacheSubnetGroupName x)
        <&> \y -> x { _ccCacheSubnetGroupName = y }
{-# INLINE ccCacheSubnetGroupName #-}

-- | A list of cache nodes that are members of the cache cluster.
ccCacheNodes :: Lens' CacheCluster ([CacheNode])
ccCacheNodes f x =
    f (_ccCacheNodes x)
        <&> \y -> x { _ccCacheNodes = y }
{-# INLINE ccCacheNodes #-}

-- | If true, then minor version patches are applied automatically; if false,
-- then automatic minor version patches are disabled.
ccAutoMinorVersionUpgrade :: Lens' CacheCluster (Maybe Bool)
ccAutoMinorVersionUpgrade f x =
    f (_ccAutoMinorVersionUpgrade x)
        <&> \y -> x { _ccAutoMinorVersionUpgrade = y }
{-# INLINE ccAutoMinorVersionUpgrade #-}

-- | A list of VPC Security Groups associated with the cache cluster.
ccSecurityGroups :: Lens' CacheCluster ([SecurityGroupMembership])
ccSecurityGroups f x =
    f (_ccSecurityGroups x)
        <&> \y -> x { _ccSecurityGroups = y }
{-# INLINE ccSecurityGroups #-}

-- | The replication group to which this cache cluster belongs. If this field is
-- empty, the cache cluster is not associated with any replication group.
ccReplicationGroupId :: Lens' CacheCluster (Maybe Text)
ccReplicationGroupId f x =
    f (_ccReplicationGroupId x)
        <&> \y -> x { _ccReplicationGroupId = y }
{-# INLINE ccReplicationGroupId #-}

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
ccSnapshotRetentionLimit :: Lens' CacheCluster (Maybe Integer)
ccSnapshotRetentionLimit f x =
    f (_ccSnapshotRetentionLimit x)
        <&> \y -> x { _ccSnapshotRetentionLimit = y }
{-# INLINE ccSnapshotRetentionLimit #-}

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster. Example: 05:00-09:00.
ccSnapshotWindow :: Lens' CacheCluster (Maybe Text)
ccSnapshotWindow f x =
    f (_ccSnapshotWindow x)
        <&> \y -> x { _ccSnapshotWindow = y }
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
cevEngine :: Lens' CacheEngineVersion (Maybe Text)
cevEngine f x =
    f (_cevEngine x)
        <&> \y -> x { _cevEngine = y }
{-# INLINE cevEngine #-}

-- | The version number of the cache engine.
cevEngineVersion :: Lens' CacheEngineVersion (Maybe Text)
cevEngineVersion f x =
    f (_cevEngineVersion x)
        <&> \y -> x { _cevEngineVersion = y }
{-# INLINE cevEngineVersion #-}

-- | The name of the cache parameter group family associated with this cache
-- engine.
cevCacheParameterGroupFamily :: Lens' CacheEngineVersion (Maybe Text)
cevCacheParameterGroupFamily f x =
    f (_cevCacheParameterGroupFamily x)
        <&> \y -> x { _cevCacheParameterGroupFamily = y }
{-# INLINE cevCacheParameterGroupFamily #-}

-- | The description of the cache engine.
cevCacheEngineDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineDescription f x =
    f (_cevCacheEngineDescription x)
        <&> \y -> x { _cevCacheEngineDescription = y }
{-# INLINE cevCacheEngineDescription #-}

-- | The description of the cache engine version.
cevCacheEngineVersionDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineVersionDescription f x =
    f (_cevCacheEngineVersionDescription x)
        <&> \y -> x { _cevCacheEngineVersionDescription = y }
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
cnCacheNodeId :: Lens' CacheNode (Maybe Text)
cnCacheNodeId f x =
    f (_cnCacheNodeId x)
        <&> \y -> x { _cnCacheNodeId = y }
{-# INLINE cnCacheNodeId #-}

-- | The current state of this cache node.
cnCacheNodeStatus :: Lens' CacheNode (Maybe Text)
cnCacheNodeStatus f x =
    f (_cnCacheNodeStatus x)
        <&> \y -> x { _cnCacheNodeStatus = y }
{-# INLINE cnCacheNodeStatus #-}

-- | The date and time when the cache node was created.
cnCacheNodeCreateTime :: Lens' CacheNode (Maybe ISO8601)
cnCacheNodeCreateTime f x =
    f (_cnCacheNodeCreateTime x)
        <&> \y -> x { _cnCacheNodeCreateTime = y }
{-# INLINE cnCacheNodeCreateTime #-}

-- | The hostname and IP address for connecting to this cache node.
cnEndpoint :: Lens' CacheNode (Maybe Endpoint)
cnEndpoint f x =
    f (_cnEndpoint x)
        <&> \y -> x { _cnEndpoint = y }
{-# INLINE cnEndpoint #-}

-- | The status of the parameter group applied to this cache node.
cnParameterGroupStatus :: Lens' CacheNode (Maybe Text)
cnParameterGroupStatus f x =
    f (_cnParameterGroupStatus x)
        <&> \y -> x { _cnParameterGroupStatus = y }
{-# INLINE cnParameterGroupStatus #-}

-- | The ID of the primary node to which this read replica node is synchronized.
-- If this field is empty, then this node is not associated with a primary
-- cache cluster.
cnSourceCacheNodeId :: Lens' CacheNode (Maybe Text)
cnSourceCacheNodeId f x =
    f (_cnSourceCacheNodeId x)
        <&> \y -> x { _cnSourceCacheNodeId = y }
{-# INLINE cnSourceCacheNodeId #-}

-- | The Availability Zone where this node was created and now resides.
cnCustomerAvailabilityZone :: Lens' CacheNode (Maybe Text)
cnCustomerAvailabilityZone f x =
    f (_cnCustomerAvailabilityZone x)
        <&> \y -> x { _cnCustomerAvailabilityZone = y }
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
cntspParameterName :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspParameterName f x =
    f (_cntspParameterName x)
        <&> \y -> x { _cntspParameterName = y }
{-# INLINE cntspParameterName #-}

-- | A description of the parameter.
cntspDescription :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDescription f x =
    f (_cntspDescription x)
        <&> \y -> x { _cntspDescription = y }
{-# INLINE cntspDescription #-}

-- | The source of the parameter value.
cntspSource :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspSource f x =
    f (_cntspSource x)
        <&> \y -> x { _cntspSource = y }
{-# INLINE cntspSource #-}

-- | The valid data type for the parameter.
cntspDataType :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDataType f x =
    f (_cntspDataType x)
        <&> \y -> x { _cntspDataType = y }
{-# INLINE cntspDataType #-}

-- | The valid range of values for the parameter.
cntspAllowedValues :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspAllowedValues f x =
    f (_cntspAllowedValues x)
        <&> \y -> x { _cntspAllowedValues = y }
{-# INLINE cntspAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
cntspIsModifiable :: Lens' CacheNodeTypeSpecificParameter (Maybe Bool)
cntspIsModifiable f x =
    f (_cntspIsModifiable x)
        <&> \y -> x { _cntspIsModifiable = y }
{-# INLINE cntspIsModifiable #-}

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspMinimumEngineVersion f x =
    f (_cntspMinimumEngineVersion x)
        <&> \y -> x { _cntspMinimumEngineVersion = y }
{-# INLINE cntspMinimumEngineVersion #-}

-- | A list of cache node types and their corresponding values for this
-- parameter.
cntspCacheNodeTypeSpecificValues :: Lens' CacheNodeTypeSpecificParameter ([CacheNodeTypeSpecificValue])
cntspCacheNodeTypeSpecificValues f x =
    f (_cntspCacheNodeTypeSpecificValues x)
        <&> \y -> x { _cntspCacheNodeTypeSpecificValues = y }
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
cntsvCacheNodeType :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvCacheNodeType f x =
    f (_cntsvCacheNodeType x)
        <&> \y -> x { _cntsvCacheNodeType = y }
{-# INLINE cntsvCacheNodeType #-}

-- | The value for the cache node type.
cntsvValue :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvValue f x =
    f (_cntsvValue x)
        <&> \y -> x { _cntsvValue = y }
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
cpgCacheParameterGroupName :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupName f x =
    f (_cpgCacheParameterGroupName x)
        <&> \y -> x { _cpgCacheParameterGroupName = y }
{-# INLINE cpgCacheParameterGroupName #-}

-- | The name of the cache parameter group family that this cache parameter
-- group is compatible with.
cpgCacheParameterGroupFamily :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupFamily f x =
    f (_cpgCacheParameterGroupFamily x)
        <&> \y -> x { _cpgCacheParameterGroupFamily = y }
{-# INLINE cpgCacheParameterGroupFamily #-}

-- | The description for this cache parameter group.
cpgDescription :: Lens' CacheParameterGroup (Maybe Text)
cpgDescription f x =
    f (_cpgDescription x)
        <&> \y -> x { _cpgDescription = y }
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
cpgsCacheParameterGroupName :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsCacheParameterGroupName f x =
    f (_cpgsCacheParameterGroupName x)
        <&> \y -> x { _cpgsCacheParameterGroupName = y }
{-# INLINE cpgsCacheParameterGroupName #-}

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus f x =
    f (_cpgsParameterApplyStatus x)
        <&> \y -> x { _cpgsParameterApplyStatus = y }
{-# INLINE cpgsParameterApplyStatus #-}

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cpgsCacheNodeIdsToReboot :: Lens' CacheParameterGroupStatus ([Text])
cpgsCacheNodeIdsToReboot f x =
    f (_cpgsCacheNodeIdsToReboot x)
        <&> \y -> x { _cpgsCacheNodeIdsToReboot = y }
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
csgOwnerId :: Lens' CacheSecurityGroup (Maybe Text)
csgOwnerId f x =
    f (_csgOwnerId x)
        <&> \y -> x { _csgOwnerId = y }
{-# INLINE csgOwnerId #-}

-- | The name of the cache security group.
csgCacheSecurityGroupName :: Lens' CacheSecurityGroup (Maybe Text)
csgCacheSecurityGroupName f x =
    f (_csgCacheSecurityGroupName x)
        <&> \y -> x { _csgCacheSecurityGroupName = y }
{-# INLINE csgCacheSecurityGroupName #-}

-- | The description of the cache security group.
csgDescription :: Lens' CacheSecurityGroup (Maybe Text)
csgDescription f x =
    f (_csgDescription x)
        <&> \y -> x { _csgDescription = y }
{-# INLINE csgDescription #-}

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
csgEC2SecurityGroups :: Lens' CacheSecurityGroup ([EC2SecurityGroup])
csgEC2SecurityGroups f x =
    f (_csgEC2SecurityGroups x)
        <&> \y -> x { _csgEC2SecurityGroups = y }
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
csgmCacheSecurityGroupName :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmCacheSecurityGroupName f x =
    f (_csgmCacheSecurityGroupName x)
        <&> \y -> x { _csgmCacheSecurityGroupName = y }
{-# INLINE csgmCacheSecurityGroupName #-}

-- | The membership status in the cache security group. The status changes when
-- a cache security group is modified, or when the cache security groups
-- assigned to a cache cluster are modified.
csgmStatus :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmStatus f x =
    f (_csgmStatus x)
        <&> \y -> x { _csgmStatus = y }
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
csiCacheSubnetGroupName :: Lens' CacheSubnetGroup (Maybe Text)
csiCacheSubnetGroupName f x =
    f (_csiCacheSubnetGroupName x)
        <&> \y -> x { _csiCacheSubnetGroupName = y }
{-# INLINE csiCacheSubnetGroupName #-}

-- | The description of the cache subnet group.
csiCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
csiCacheSubnetGroupDescription f x =
    f (_csiCacheSubnetGroupDescription x)
        <&> \y -> x { _csiCacheSubnetGroupDescription = y }
{-# INLINE csiCacheSubnetGroupDescription #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
csiVpcId :: Lens' CacheSubnetGroup (Maybe Text)
csiVpcId f x =
    f (_csiVpcId x)
        <&> \y -> x { _csiVpcId = y }
{-# INLINE csiVpcId #-}

-- | A list of subnets associated with the cache subnet group.
csiSubnets :: Lens' CacheSubnetGroup ([Subnet])
csiSubnets f x =
    f (_csiSubnets x)
        <&> \y -> x { _csiSubnets = y }
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
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus f x =
    f (_ecsgStatus x)
        <&> \y -> x { _ecsgStatus = y }
{-# INLINE ecsgStatus #-}

-- | The name of the Amazon EC2 security group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName f x =
    f (_ecsgEC2SecurityGroupName x)
        <&> \y -> x { _ecsgEC2SecurityGroupName = y }
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | The AWS account ID of the Amazon EC2 security group owner.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId f x =
    f (_ecsgEC2SecurityGroupOwnerId x)
        <&> \y -> x { _ecsgEC2SecurityGroupOwnerId = y }
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
eAddress :: Lens' Endpoint (Maybe Text)
eAddress f x =
    f (_eAddress x)
        <&> \y -> x { _eAddress = y }
{-# INLINE eAddress #-}

-- | The port number that the cache engine is listening on.
ePort :: Lens' Endpoint (Maybe Integer)
ePort f x =
    f (_ePort x)
        <&> \y -> x { _ePort = y }
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
edCacheParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edCacheParameterGroupFamily f x =
    f (_edCacheParameterGroupFamily x)
        <&> \y -> x { _edCacheParameterGroupFamily = y }
{-# INLINE edCacheParameterGroupFamily #-}

-- | Provides an identifier to allow retrieval of paginated results.
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker f x =
    f (_edMarker x)
        <&> \y -> x { _edMarker = y }
{-# INLINE edMarker #-}

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults ([Parameter])
edParameters f x =
    f (_edParameters x)
        <&> \y -> x { _edParameters = y }
{-# INLINE edParameters #-}

-- | A list of parameters specific to a particular cache node type. Each element
-- in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters :: Lens' EngineDefaults ([CacheNodeTypeSpecificParameter])
edCacheNodeTypeSpecificParameters f x =
    f (_edCacheNodeTypeSpecificParameters x)
        <&> \y -> x { _edCacheNodeTypeSpecificParameters = y }
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
exSourceIdentifier :: Lens' Event (Maybe Text)
exSourceIdentifier f x =
    f (_exSourceIdentifier x)
        <&> \y -> x { _exSourceIdentifier = y }
{-# INLINE exSourceIdentifier #-}

-- | Specifies the origin of this event - a cache cluster, a parameter group, a
-- security group, etc.
exSourceType :: Lens' Event (Maybe SourceType)
exSourceType f x =
    f (_exSourceType x)
        <&> \y -> x { _exSourceType = y }
{-# INLINE exSourceType #-}

-- | The text of the event.
exMessage :: Lens' Event (Maybe Text)
exMessage f x =
    f (_exMessage x)
        <&> \y -> x { _exMessage = y }
{-# INLINE exMessage #-}

-- | The date and time when the event occurred.
exDate :: Lens' Event (Maybe ISO8601)
exDate f x =
    f (_exDate x)
        <&> \y -> x { _exDate = y }
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
ngNodeGroupId :: Lens' NodeGroup (Maybe Text)
ngNodeGroupId f x =
    f (_ngNodeGroupId x)
        <&> \y -> x { _ngNodeGroupId = y }
{-# INLINE ngNodeGroupId #-}

-- | The current state of this replication group - creating, available, etc.
ngStatus :: Lens' NodeGroup (Maybe Text)
ngStatus f x =
    f (_ngStatus x)
        <&> \y -> x { _ngStatus = y }
{-# INLINE ngStatus #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ngPrimaryEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngPrimaryEndpoint f x =
    f (_ngPrimaryEndpoint x)
        <&> \y -> x { _ngPrimaryEndpoint = y }
{-# INLINE ngPrimaryEndpoint #-}

-- | A list containing information about individual nodes within the node group.
ngNodeGroupMembers :: Lens' NodeGroup ([NodeGroupMember])
ngNodeGroupMembers f x =
    f (_ngNodeGroupMembers x)
        <&> \y -> x { _ngNodeGroupMembers = y }
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
ngmCacheClusterId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheClusterId f x =
    f (_ngmCacheClusterId x)
        <&> \y -> x { _ngmCacheClusterId = y }
{-# INLINE ngmCacheClusterId #-}

-- | The ID of the node within its cache cluster. A node ID is a numeric
-- identifier (0001, 0002, etc.).
ngmCacheNodeId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheNodeId f x =
    f (_ngmCacheNodeId x)
        <&> \y -> x { _ngmCacheNodeId = y }
{-# INLINE ngmCacheNodeId #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ngmReadEndpoint :: Lens' NodeGroupMember (Maybe Endpoint)
ngmReadEndpoint f x =
    f (_ngmReadEndpoint x)
        <&> \y -> x { _ngmReadEndpoint = y }
{-# INLINE ngmReadEndpoint #-}

-- | The name of the Availability Zone in which the node is located.
ngmPreferredAvailabilityZone :: Lens' NodeGroupMember (Maybe Text)
ngmPreferredAvailabilityZone f x =
    f (_ngmPreferredAvailabilityZone x)
        <&> \y -> x { _ngmPreferredAvailabilityZone = y }
{-# INLINE ngmPreferredAvailabilityZone #-}

-- | The role that is currently assigned to the node - primary or replica.
ngmCurrentRole :: Lens' NodeGroupMember (Maybe Text)
ngmCurrentRole f x =
    f (_ngmCurrentRole x)
        <&> \y -> x { _ngmCurrentRole = y }
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
nsCacheNodeId :: Lens' NodeSnapshot (Maybe Text)
nsCacheNodeId f x =
    f (_nsCacheNodeId x)
        <&> \y -> x { _nsCacheNodeId = y }
{-# INLINE nsCacheNodeId #-}

-- | The size of the cache on the source cache node.
nsCacheSize :: Lens' NodeSnapshot (Maybe Text)
nsCacheSize f x =
    f (_nsCacheSize x)
        <&> \y -> x { _nsCacheSize = y }
{-# INLINE nsCacheSize #-}

-- | The date and time when the cache node was created in the source cache
-- cluster.
nsCacheNodeCreateTime :: Lens' NodeSnapshot (Maybe ISO8601)
nsCacheNodeCreateTime f x =
    f (_nsCacheNodeCreateTime x)
        <&> \y -> x { _nsCacheNodeCreateTime = y }
{-# INLINE nsCacheNodeCreateTime #-}

-- | The date and time when the source node's metadata and cache data set was
-- obtained for the snapshot.
nsSnapshotCreateTime :: Lens' NodeSnapshot (Maybe ISO8601)
nsSnapshotCreateTime f x =
    f (_nsSnapshotCreateTime x)
        <&> \y -> x { _nsSnapshotCreateTime = y }
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
ncTopicArn :: Lens' NotificationConfiguration (Maybe Text)
ncTopicArn f x =
    f (_ncTopicArn x)
        <&> \y -> x { _ncTopicArn = y }
{-# INLINE ncTopicArn #-}

-- | The current state of the topic.
ncTopicStatus :: Lens' NotificationConfiguration (Maybe Text)
ncTopicStatus f x =
    f (_ncTopicStatus x)
        <&> \y -> x { _ncTopicStatus = y }
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
prParameterName :: Lens' Parameter (Maybe Text)
prParameterName f x =
    f (_prParameterName x)
        <&> \y -> x { _prParameterName = y }
{-# INLINE prParameterName #-}

-- | The value of the parameter.
prParameterValue :: Lens' Parameter (Maybe Text)
prParameterValue f x =
    f (_prParameterValue x)
        <&> \y -> x { _prParameterValue = y }
{-# INLINE prParameterValue #-}

-- | A description of the parameter.
prDescription :: Lens' Parameter (Maybe Text)
prDescription f x =
    f (_prDescription x)
        <&> \y -> x { _prDescription = y }
{-# INLINE prDescription #-}

-- | The source of the parameter.
prSource :: Lens' Parameter (Maybe Text)
prSource f x =
    f (_prSource x)
        <&> \y -> x { _prSource = y }
{-# INLINE prSource #-}

-- | The valid data type for the parameter.
prDataType :: Lens' Parameter (Maybe Text)
prDataType f x =
    f (_prDataType x)
        <&> \y -> x { _prDataType = y }
{-# INLINE prDataType #-}

-- | The valid range of values for the parameter.
prAllowedValues :: Lens' Parameter (Maybe Text)
prAllowedValues f x =
    f (_prAllowedValues x)
        <&> \y -> x { _prAllowedValues = y }
{-# INLINE prAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
prIsModifiable :: Lens' Parameter (Maybe Bool)
prIsModifiable f x =
    f (_prIsModifiable x)
        <&> \y -> x { _prIsModifiable = y }
{-# INLINE prIsModifiable #-}

-- | The earliest cache engine version to which the parameter can apply.
prMinimumEngineVersion :: Lens' Parameter (Maybe Text)
prMinimumEngineVersion f x =
    f (_prMinimumEngineVersion x)
        <&> \y -> x { _prMinimumEngineVersion = y }
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
pnvParameterName :: Lens' ParameterNameValue (Maybe Text)
pnvParameterName f x =
    f (_pnvParameterName x)
        <&> \y -> x { _pnvParameterName = y }
{-# INLINE pnvParameterName #-}

-- | The value of the parameter.
pnvParameterValue :: Lens' ParameterNameValue (Maybe Text)
pnvParameterValue f x =
    f (_pnvParameterValue x)
        <&> \y -> x { _pnvParameterValue = y }
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
pmvNumCacheNodes :: Lens' PendingModifiedValues (Maybe Integer)
pmvNumCacheNodes f x =
    f (_pmvNumCacheNodes x)
        <&> \y -> x { _pmvNumCacheNodes = y }
{-# INLINE pmvNumCacheNodes #-}

-- | A list of cache node IDs that are being removed (or will be removed) from
-- the cache cluster. A node ID is a numeric identifier (0001, 0002, etc.).
pmvCacheNodeIdsToRemove :: Lens' PendingModifiedValues ([Text])
pmvCacheNodeIdsToRemove f x =
    f (_pmvCacheNodeIdsToRemove x)
        <&> \y -> x { _pmvCacheNodeIdsToRemove = y }
{-# INLINE pmvCacheNodeIdsToRemove #-}

-- | The new cache engine version that the cache cluster will run.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion f x =
    f (_pmvEngineVersion x)
        <&> \y -> x { _pmvEngineVersion = y }
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
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount f x =
    f (_rcRecurringChargeAmount x)
        <&> \y -> x { _rcRecurringChargeAmount = y }
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency f x =
    f (_rcRecurringChargeFrequency x)
        <&> \y -> x { _rcRecurringChargeFrequency = y }
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
rgReplicationGroupId :: Lens' ReplicationGroup (Maybe Text)
rgReplicationGroupId f x =
    f (_rgReplicationGroupId x)
        <&> \y -> x { _rgReplicationGroupId = y }
{-# INLINE rgReplicationGroupId #-}

-- | The description of the replication group.
rgDescription :: Lens' ReplicationGroup (Maybe Text)
rgDescription f x =
    f (_rgDescription x)
        <&> \y -> x { _rgDescription = y }
{-# INLINE rgDescription #-}

-- | The current state of this replication group - creating, available, etc.
rgStatus :: Lens' ReplicationGroup (Maybe Text)
rgStatus f x =
    f (_rgStatus x)
        <&> \y -> x { _rgStatus = y }
{-# INLINE rgStatus #-}

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
rgPendingModifiedValues :: Lens' ReplicationGroup (Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues f x =
    f (_rgPendingModifiedValues x)
        <&> \y -> x { _rgPendingModifiedValues = y }
{-# INLINE rgPendingModifiedValues #-}

-- | The names of all the cache clusters that are part of this replication
-- group.
rgMemberClusters :: Lens' ReplicationGroup ([Text])
rgMemberClusters f x =
    f (_rgMemberClusters x)
        <&> \y -> x { _rgMemberClusters = y }
{-# INLINE rgMemberClusters #-}

-- | A single element list with information about the nodes in the replication
-- group.
rgNodeGroups :: Lens' ReplicationGroup ([NodeGroup])
rgNodeGroups f x =
    f (_rgNodeGroups x)
        <&> \y -> x { _rgNodeGroups = y }
{-# INLINE rgNodeGroups #-}

-- | The cache cluster ID that is used as the daily snapshot source for the
-- replication group.
rgSnapshottingClusterId :: Lens' ReplicationGroup (Maybe Text)
rgSnapshottingClusterId f x =
    f (_rgSnapshottingClusterId x)
        <&> \y -> x { _rgSnapshottingClusterId = y }
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
rcnReservedCacheNodeId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodeId f x =
    f (_rcnReservedCacheNodeId x)
        <&> \y -> x { _rcnReservedCacheNodeId = y }
{-# INLINE rcnReservedCacheNodeId #-}

-- | The offering identifier.
rcnReservedCacheNodesOfferingId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodesOfferingId f x =
    f (_rcnReservedCacheNodesOfferingId x)
        <&> \y -> x { _rcnReservedCacheNodesOfferingId = y }
{-# INLINE rcnReservedCacheNodesOfferingId #-}

-- | The cache node type for the reserved cache nodes.
rcnCacheNodeType :: Lens' ReservedCacheNode (Maybe Text)
rcnCacheNodeType f x =
    f (_rcnCacheNodeType x)
        <&> \y -> x { _rcnCacheNodeType = y }
{-# INLINE rcnCacheNodeType #-}

-- | The time the reservation started.
rcnStartTime :: Lens' ReservedCacheNode (Maybe ISO8601)
rcnStartTime f x =
    f (_rcnStartTime x)
        <&> \y -> x { _rcnStartTime = y }
{-# INLINE rcnStartTime #-}

-- | The duration of the reservation in seconds.
rcnDuration :: Lens' ReservedCacheNode (Maybe Integer)
rcnDuration f x =
    f (_rcnDuration x)
        <&> \y -> x { _rcnDuration = y }
{-# INLINE rcnDuration #-}

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice :: Lens' ReservedCacheNode (Maybe Double)
rcnFixedPrice f x =
    f (_rcnFixedPrice x)
        <&> \y -> x { _rcnFixedPrice = y }
{-# INLINE rcnFixedPrice #-}

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice :: Lens' ReservedCacheNode (Maybe Double)
rcnUsagePrice f x =
    f (_rcnUsagePrice x)
        <&> \y -> x { _rcnUsagePrice = y }
{-# INLINE rcnUsagePrice #-}

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount :: Lens' ReservedCacheNode (Maybe Integer)
rcnCacheNodeCount f x =
    f (_rcnCacheNodeCount x)
        <&> \y -> x { _rcnCacheNodeCount = y }
{-# INLINE rcnCacheNodeCount #-}

-- | The description of the reserved cache node.
rcnProductDescription :: Lens' ReservedCacheNode (Maybe Text)
rcnProductDescription f x =
    f (_rcnProductDescription x)
        <&> \y -> x { _rcnProductDescription = y }
{-# INLINE rcnProductDescription #-}

-- | The offering type of this reserved cache node.
rcnOfferingType :: Lens' ReservedCacheNode (Maybe Text)
rcnOfferingType f x =
    f (_rcnOfferingType x)
        <&> \y -> x { _rcnOfferingType = y }
{-# INLINE rcnOfferingType #-}

-- | The state of the reserved cache node.
rcnState :: Lens' ReservedCacheNode (Maybe Text)
rcnState f x =
    f (_rcnState x)
        <&> \y -> x { _rcnState = y }
{-# INLINE rcnState #-}

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges :: Lens' ReservedCacheNode ([RecurringCharge])
rcnRecurringCharges f x =
    f (_rcnRecurringCharges x)
        <&> \y -> x { _rcnRecurringCharges = y }
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
rcnoReservedCacheNodesOfferingId :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoReservedCacheNodesOfferingId f x =
    f (_rcnoReservedCacheNodesOfferingId x)
        <&> \y -> x { _rcnoReservedCacheNodesOfferingId = y }
{-# INLINE rcnoReservedCacheNodesOfferingId #-}

-- | The cache node type for the reserved cache node.
rcnoCacheNodeType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoCacheNodeType f x =
    f (_rcnoCacheNodeType x)
        <&> \y -> x { _rcnoCacheNodeType = y }
{-# INLINE rcnoCacheNodeType #-}

-- | The duration of the offering. in seconds.
rcnoDuration :: Lens' ReservedCacheNodesOffering (Maybe Integer)
rcnoDuration f x =
    f (_rcnoDuration x)
        <&> \y -> x { _rcnoDuration = y }
{-# INLINE rcnoDuration #-}

-- | The fixed price charged for this offering.
rcnoFixedPrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoFixedPrice f x =
    f (_rcnoFixedPrice x)
        <&> \y -> x { _rcnoFixedPrice = y }
{-# INLINE rcnoFixedPrice #-}

-- | The hourly price charged for this offering.
rcnoUsagePrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoUsagePrice f x =
    f (_rcnoUsagePrice x)
        <&> \y -> x { _rcnoUsagePrice = y }
{-# INLINE rcnoUsagePrice #-}

-- | The cache engine used by the offering.
rcnoProductDescription :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoProductDescription f x =
    f (_rcnoProductDescription x)
        <&> \y -> x { _rcnoProductDescription = y }
{-# INLINE rcnoProductDescription #-}

-- | The offering type.
rcnoOfferingType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoOfferingType f x =
    f (_rcnoOfferingType x)
        <&> \y -> x { _rcnoOfferingType = y }
{-# INLINE rcnoOfferingType #-}

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges :: Lens' ReservedCacheNodesOffering ([RecurringCharge])
rcnoRecurringCharges f x =
    f (_rcnoRecurringCharges x)
        <&> \y -> x { _rcnoRecurringCharges = y }
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
sgmSecurityGroupId :: Lens' SecurityGroupMembership (Maybe Text)
sgmSecurityGroupId f x =
    f (_sgmSecurityGroupId x)
        <&> \y -> x { _sgmSecurityGroupId = y }
{-# INLINE sgmSecurityGroupId #-}

-- | The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cache cluster are modified.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus f x =
    f (_sgmStatus x)
        <&> \y -> x { _sgmStatus = y }
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
stSnapshotName :: Lens' Snapshot (Maybe Text)
stSnapshotName f x =
    f (_stSnapshotName x)
        <&> \y -> x { _stSnapshotName = y }
{-# INLINE stSnapshotName #-}

-- | The user-supplied identifier of the source cache cluster.
stCacheClusterId :: Lens' Snapshot (Maybe Text)
stCacheClusterId f x =
    f (_stCacheClusterId x)
        <&> \y -> x { _stCacheClusterId = y }
{-# INLINE stCacheClusterId #-}

-- | The status of the snapshot. Valid values: creating | available | restoring
-- | copying | deleting.
stSnapshotStatus :: Lens' Snapshot (Maybe Text)
stSnapshotStatus f x =
    f (_stSnapshotStatus x)
        <&> \y -> x { _stSnapshotStatus = y }
{-# INLINE stSnapshotStatus #-}

-- | Indicates whether the snapshot is from an automatic backup (automated) or
-- was created manually (manual).
stSnapshotSource :: Lens' Snapshot (Maybe Text)
stSnapshotSource f x =
    f (_stSnapshotSource x)
        <&> \y -> x { _stSnapshotSource = y }
{-# INLINE stSnapshotSource #-}

-- | The name of the compute and memory capacity node type for the source cache
-- cluster.
stCacheNodeType :: Lens' Snapshot (Maybe Text)
stCacheNodeType f x =
    f (_stCacheNodeType x)
        <&> \y -> x { _stCacheNodeType = y }
{-# INLINE stCacheNodeType #-}

-- | The name of the cache engine (memcached or redis) used by the source cache
-- cluster.
stEngine :: Lens' Snapshot (Maybe Text)
stEngine f x =
    f (_stEngine x)
        <&> \y -> x { _stEngine = y }
{-# INLINE stEngine #-}

-- | The version of the cache engine version that is used by the source cache
-- cluster.
stEngineVersion :: Lens' Snapshot (Maybe Text)
stEngineVersion f x =
    f (_stEngineVersion x)
        <&> \y -> x { _stEngineVersion = y }
{-# INLINE stEngineVersion #-}

-- | The number of cache nodes in the source cache cluster.
stNumCacheNodes :: Lens' Snapshot (Maybe Integer)
stNumCacheNodes f x =
    f (_stNumCacheNodes x)
        <&> \y -> x { _stNumCacheNodes = y }
{-# INLINE stNumCacheNodes #-}

-- | The name of the Availability Zone in which the source cache cluster is
-- located.
stPreferredAvailabilityZone :: Lens' Snapshot (Maybe Text)
stPreferredAvailabilityZone f x =
    f (_stPreferredAvailabilityZone x)
        <&> \y -> x { _stPreferredAvailabilityZone = y }
{-# INLINE stPreferredAvailabilityZone #-}

-- | The date and time when the source cache cluster was created.
stCacheClusterCreateTime :: Lens' Snapshot (Maybe ISO8601)
stCacheClusterCreateTime f x =
    f (_stCacheClusterCreateTime x)
        <&> \y -> x { _stCacheClusterCreateTime = y }
{-# INLINE stCacheClusterCreateTime #-}

-- | The time range (in UTC) during which weekly system maintenance can occur on
-- the source cache cluster.
stPreferredMaintenanceWindow :: Lens' Snapshot (Maybe Text)
stPreferredMaintenanceWindow f x =
    f (_stPreferredMaintenanceWindow x)
        <&> \y -> x { _stPreferredMaintenanceWindow = y }
{-# INLINE stPreferredMaintenanceWindow #-}

-- | The Amazon Resource Name (ARN) for the topic used by the source cache
-- cluster for publishing notifications.
stTopicArn :: Lens' Snapshot (Maybe Text)
stTopicArn f x =
    f (_stTopicArn x)
        <&> \y -> x { _stTopicArn = y }
{-# INLINE stTopicArn #-}

-- | The port number used by each cache nodes in the source cache cluster.
stPort :: Lens' Snapshot (Maybe Integer)
stPort f x =
    f (_stPort x)
        <&> \y -> x { _stPort = y }
{-# INLINE stPort #-}

-- | The cache parameter group that is associated with the source cache cluster.
stCacheParameterGroupName :: Lens' Snapshot (Maybe Text)
stCacheParameterGroupName f x =
    f (_stCacheParameterGroupName x)
        <&> \y -> x { _stCacheParameterGroupName = y }
{-# INLINE stCacheParameterGroupName #-}

-- | The name of the cache subnet group associated with the source cache
-- cluster.
stCacheSubnetGroupName :: Lens' Snapshot (Maybe Text)
stCacheSubnetGroupName f x =
    f (_stCacheSubnetGroupName x)
        <&> \y -> x { _stCacheSubnetGroupName = y }
{-# INLINE stCacheSubnetGroupName #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cache cluster.
stVpcId :: Lens' Snapshot (Maybe Text)
stVpcId f x =
    f (_stVpcId x)
        <&> \y -> x { _stVpcId = y }
{-# INLINE stVpcId #-}

-- | For the source cache cluster, indicates whether minor version patches are
-- applied automatically (true) or not (false).
stAutoMinorVersionUpgrade :: Lens' Snapshot (Maybe Bool)
stAutoMinorVersionUpgrade f x =
    f (_stAutoMinorVersionUpgrade x)
        <&> \y -> x { _stAutoMinorVersionUpgrade = y }
{-# INLINE stAutoMinorVersionUpgrade #-}

-- | For an automatic snapshot, the number of days for which ElastiCache will
-- retain the snapshot before deleting it. For manual snapshots, this field
-- reflects the SnapshotRetentionLimit for the source cache cluster when the
-- snapshot was created. This field is otherwise ignored: Manual snapshots do
-- not expire, and can only be deleted using the DeleteSnapshot action.
-- ImportantIf the value of SnapshotRetentionLimit is set to zero (0), backups
-- are turned off.
stSnapshotRetentionLimit :: Lens' Snapshot (Maybe Integer)
stSnapshotRetentionLimit f x =
    f (_stSnapshotRetentionLimit x)
        <&> \y -> x { _stSnapshotRetentionLimit = y }
{-# INLINE stSnapshotRetentionLimit #-}

-- | The daily time range during which ElastiCache takes daily snapshots of the
-- source cache cluster.
stSnapshotWindow :: Lens' Snapshot (Maybe Text)
stSnapshotWindow f x =
    f (_stSnapshotWindow x)
        <&> \y -> x { _stSnapshotWindow = y }
{-# INLINE stSnapshotWindow #-}

-- | A list of the cache nodes in the source cache cluster.
stNodeSnapshots :: Lens' Snapshot ([NodeSnapshot])
stNodeSnapshots f x =
    f (_stNodeSnapshots x)
        <&> \y -> x { _stNodeSnapshots = y }
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
sssssuSubnetIdentifier :: Lens' Subnet (Maybe Text)
sssssuSubnetIdentifier f x =
    f (_sssssuSubnetIdentifier x)
        <&> \y -> x { _sssssuSubnetIdentifier = y }
{-# INLINE sssssuSubnetIdentifier #-}

-- | The Availability Zone associated with the subnet.
sssssuSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sssssuSubnetAvailabilityZone f x =
    f (_sssssuSubnetAvailabilityZone x)
        <&> \y -> x { _sssssuSubnetAvailabilityZone = y }
{-# INLINE sssssuSubnetAvailabilityZone #-}

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def
