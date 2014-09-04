{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
    , AvailabilityZone
    , mkAvailabilityZone
    , azName

    -- * ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues
    , mkReplicationGroupPendingModifiedValues
    , rgpmvPrimaryClusterId

    -- * CacheCluster
    , CacheCluster
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
    , CacheEngineVersion
    , cevEngine
    , cevEngineVersion
    , cevCacheParameterGroupFamily
    , cevCacheEngineDescription
    , cevCacheEngineVersionDescription

    -- * CacheNode
    , CacheNode
    , mkCacheNode
    , cnCacheNodeId
    , cnCacheNodeStatus
    , cnCacheNodeCreateTime
    , cnEndpoint
    , cnParameterGroupStatus
    , cnSourceCacheNodeId
    , cnCustomerAvailabilityZone

    -- * CacheNodeTypeSpecificParameter
    , CacheNodeTypeSpecificParameter
    , cntspParameterName
    , cntspDescription
    , cntspSource
    , cntspDataType
    , cntspAllowedValues
    , cntspIsModifiable
    , cntspMinimumEngineVersion
    , cntspCacheNodeTypeSpecificValues

    -- * CacheNodeTypeSpecificValue
    , CacheNodeTypeSpecificValue
    , mkCacheNodeTypeSpecificValue
    , cntsvCacheNodeType
    , cntsvValue

    -- * CacheParameterGroup
    , CacheParameterGroup
    , cpgCacheParameterGroupName
    , cpgCacheParameterGroupFamily
    , cpgDescription

    -- * CacheParameterGroupStatus
    , CacheParameterGroupStatus
    , mkCacheParameterGroupStatus
    , cpgsCacheParameterGroupName
    , cpgsParameterApplyStatus
    , cpgsCacheNodeIdsToReboot

    -- * CacheSecurityGroup
    , CacheSecurityGroup
    , csgOwnerId
    , csgCacheSecurityGroupName
    , csgDescription
    , csgEC2SecurityGroups

    -- * CacheSecurityGroupMembership
    , CacheSecurityGroupMembership
    , mkCacheSecurityGroupMembership
    , csgmCacheSecurityGroupName
    , csgmStatus

    -- * CacheSubnetGroup
    , CacheSubnetGroup
    , csiCacheSubnetGroupName
    , csiCacheSubnetGroupDescription
    , csiVpcId
    , csiSubnets

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , mkEC2SecurityGroup
    , ecsgStatus
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId

    -- * Endpoint
    , Endpoint
    , mkEndpoint
    , eAddress
    , ePort

    -- * EngineDefaults
    , EngineDefaults
    , edCacheParameterGroupFamily
    , edMarker
    , edParameters
    , edCacheNodeTypeSpecificParameters

    -- * Event
    , Event
    , exSourceIdentifier
    , exSourceType
    , exMessage
    , exDate

    -- * NodeGroup
    , NodeGroup
    , mkNodeGroup
    , ngNodeGroupId
    , ngStatus
    , ngPrimaryEndpoint
    , ngNodeGroupMembers

    -- * NodeGroupMember
    , NodeGroupMember
    , mkNodeGroupMember
    , ngmCacheClusterId
    , ngmCacheNodeId
    , ngmReadEndpoint
    , ngmPreferredAvailabilityZone
    , ngmCurrentRole

    -- * NodeSnapshot
    , NodeSnapshot
    , mkNodeSnapshot
    , nsCacheNodeId
    , nsCacheSize
    , nsCacheNodeCreateTime
    , nsSnapshotCreateTime

    -- * NotificationConfiguration
    , NotificationConfiguration
    , mkNotificationConfiguration
    , ncTopicArn
    , ncTopicStatus

    -- * Parameter
    , Parameter
    , prParameterName
    , prParameterValue
    , prDescription
    , prSource
    , prDataType
    , prAllowedValues
    , prIsModifiable
    , prMinimumEngineVersion

    -- * ParameterNameValue
    , ParameterNameValue
    , mkParameterNameValue
    , pnvParameterName
    , pnvParameterValue

    -- * PendingModifiedValues
    , PendingModifiedValues
    , mkPendingModifiedValues
    , pmvNumCacheNodes
    , pmvCacheNodeIdsToRemove
    , pmvEngineVersion

    -- * RecurringCharge
    , RecurringCharge
    , mkRecurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReplicationGroup
    , ReplicationGroup
    , rgReplicationGroupId
    , rgDescription
    , rgStatus
    , rgPendingModifiedValues
    , rgMemberClusters
    , rgNodeGroups
    , rgSnapshottingClusterId

    -- * ReservedCacheNode
    , ReservedCacheNode
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
    , ReservedCacheNodesOffering
    , rcnoReservedCacheNodesOfferingId
    , rcnoCacheNodeType
    , rcnoDuration
    , rcnoFixedPrice
    , rcnoUsagePrice
    , rcnoProductDescription
    , rcnoOfferingType
    , rcnoRecurringCharges

    -- * SecurityGroupMembership
    , SecurityGroupMembership
    , mkSecurityGroupMembership
    , sgmSecurityGroupId
    , sgmStatus

    -- * Snapshot
    , Snapshot
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
    , Subnet
    , mkSubnet
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
azName = lens _azName (\s a -> s { _azName = a })
{-# INLINE azName #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type to populate a request.
mkAvailabilityZone :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone
    { _azName = Nothing
    }
{-# INLINE mkAvailabilityZone #-}

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
rgpmvPrimaryClusterId = lens _rgpmvPrimaryClusterId (\s a -> s { _rgpmvPrimaryClusterId = a })
{-# INLINE rgpmvPrimaryClusterId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReplicationGroupPendingModifiedValues' data type to populate a request.
mkReplicationGroupPendingModifiedValues :: ReplicationGroupPendingModifiedValues
mkReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { _rgpmvPrimaryClusterId = Nothing
    }
{-# INLINE mkReplicationGroupPendingModifiedValues #-}

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
ccCacheClusterId = lens _ccCacheClusterId (\s a -> s { _ccCacheClusterId = a })
{-# INLINE ccCacheClusterId #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ccConfigurationEndpoint :: Lens' CacheCluster (Maybe Endpoint)
ccConfigurationEndpoint = lens _ccConfigurationEndpoint (\s a -> s { _ccConfigurationEndpoint = a })
{-# INLINE ccConfigurationEndpoint #-}

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
ccClientDownloadLandingPage :: Lens' CacheCluster (Maybe Text)
ccClientDownloadLandingPage = lens _ccClientDownloadLandingPage (\s a -> s { _ccClientDownloadLandingPage = a })
{-# INLINE ccClientDownloadLandingPage #-}

-- | The name of the compute and memory capacity node type for the cache
-- cluster.
ccCacheNodeType :: Lens' CacheCluster (Maybe Text)
ccCacheNodeType = lens _ccCacheNodeType (\s a -> s { _ccCacheNodeType = a })
{-# INLINE ccCacheNodeType #-}

-- | The name of the cache engine (memcached or redis) to be used for this cache
-- cluster.
ccEngine :: Lens' CacheCluster (Maybe Text)
ccEngine = lens _ccEngine (\s a -> s { _ccEngine = a })
{-# INLINE ccEngine #-}

-- | The version of the cache engine version that is used in this cache cluster.
ccEngineVersion :: Lens' CacheCluster (Maybe Text)
ccEngineVersion = lens _ccEngineVersion (\s a -> s { _ccEngineVersion = a })
{-# INLINE ccEngineVersion #-}

-- | The current state of this cache cluster - creating, available, etc.
ccCacheClusterStatus :: Lens' CacheCluster (Maybe Text)
ccCacheClusterStatus = lens _ccCacheClusterStatus (\s a -> s { _ccCacheClusterStatus = a })
{-# INLINE ccCacheClusterStatus #-}

-- | The number of cache nodes in the cache cluster.
ccNumCacheNodes :: Lens' CacheCluster (Maybe Integer)
ccNumCacheNodes = lens _ccNumCacheNodes (\s a -> s { _ccNumCacheNodes = a })
{-# INLINE ccNumCacheNodes #-}

-- | The name of the Availability Zone in which the cache cluster is located or
-- "Multiple" if the cache nodes are located in different Availability Zones.
ccPreferredAvailabilityZone :: Lens' CacheCluster (Maybe Text)
ccPreferredAvailabilityZone = lens _ccPreferredAvailabilityZone (\s a -> s { _ccPreferredAvailabilityZone = a })
{-# INLINE ccPreferredAvailabilityZone #-}

-- | The date and time when the cache cluster was created.
ccCacheClusterCreateTime :: Lens' CacheCluster (Maybe ISO8601)
ccCacheClusterCreateTime = lens _ccCacheClusterCreateTime (\s a -> s { _ccCacheClusterCreateTime = a })
{-# INLINE ccCacheClusterCreateTime #-}

-- | The time range (in UTC) during which weekly system maintenance can occur.
ccPreferredMaintenanceWindow :: Lens' CacheCluster (Maybe Text)
ccPreferredMaintenanceWindow = lens _ccPreferredMaintenanceWindow (\s a -> s { _ccPreferredMaintenanceWindow = a })
{-# INLINE ccPreferredMaintenanceWindow #-}

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
ccPendingModifiedValues :: Lens' CacheCluster (Maybe PendingModifiedValues)
ccPendingModifiedValues = lens _ccPendingModifiedValues (\s a -> s { _ccPendingModifiedValues = a })
{-# INLINE ccPendingModifiedValues #-}

-- | Describes a notification topic and its status. Notification topics are used
-- for publishing ElastiCache events to subscribers using Amazon Simple
-- Notification Service (SNS).
ccNotificationConfiguration :: Lens' CacheCluster (Maybe NotificationConfiguration)
ccNotificationConfiguration = lens _ccNotificationConfiguration (\s a -> s { _ccNotificationConfiguration = a })
{-# INLINE ccNotificationConfiguration #-}

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
ccCacheSecurityGroups :: Lens' CacheCluster ([CacheSecurityGroupMembership])
ccCacheSecurityGroups = lens _ccCacheSecurityGroups (\s a -> s { _ccCacheSecurityGroups = a })
{-# INLINE ccCacheSecurityGroups #-}

-- | The status of the cache parameter group.
ccCacheParameterGroup :: Lens' CacheCluster (Maybe CacheParameterGroupStatus)
ccCacheParameterGroup = lens _ccCacheParameterGroup (\s a -> s { _ccCacheParameterGroup = a })
{-# INLINE ccCacheParameterGroup #-}

-- | The name of the cache subnet group associated with the cache cluster.
ccCacheSubnetGroupName :: Lens' CacheCluster (Maybe Text)
ccCacheSubnetGroupName = lens _ccCacheSubnetGroupName (\s a -> s { _ccCacheSubnetGroupName = a })
{-# INLINE ccCacheSubnetGroupName #-}

-- | A list of cache nodes that are members of the cache cluster.
ccCacheNodes :: Lens' CacheCluster ([CacheNode])
ccCacheNodes = lens _ccCacheNodes (\s a -> s { _ccCacheNodes = a })
{-# INLINE ccCacheNodes #-}

-- | If true, then minor version patches are applied automatically; if false,
-- then automatic minor version patches are disabled.
ccAutoMinorVersionUpgrade :: Lens' CacheCluster (Maybe Bool)
ccAutoMinorVersionUpgrade = lens _ccAutoMinorVersionUpgrade (\s a -> s { _ccAutoMinorVersionUpgrade = a })
{-# INLINE ccAutoMinorVersionUpgrade #-}

-- | A list of VPC Security Groups associated with the cache cluster.
ccSecurityGroups :: Lens' CacheCluster ([SecurityGroupMembership])
ccSecurityGroups = lens _ccSecurityGroups (\s a -> s { _ccSecurityGroups = a })
{-# INLINE ccSecurityGroups #-}

-- | The replication group to which this cache cluster belongs. If this field is
-- empty, the cache cluster is not associated with any replication group.
ccReplicationGroupId :: Lens' CacheCluster (Maybe Text)
ccReplicationGroupId = lens _ccReplicationGroupId (\s a -> s { _ccReplicationGroupId = a })
{-# INLINE ccReplicationGroupId #-}

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
ccSnapshotRetentionLimit :: Lens' CacheCluster (Maybe Integer)
ccSnapshotRetentionLimit = lens _ccSnapshotRetentionLimit (\s a -> s { _ccSnapshotRetentionLimit = a })
{-# INLINE ccSnapshotRetentionLimit #-}

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster. Example: 05:00-09:00.
ccSnapshotWindow :: Lens' CacheCluster (Maybe Text)
ccSnapshotWindow = lens _ccSnapshotWindow (\s a -> s { _ccSnapshotWindow = a })
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
cevEngine = lens _cevEngine (\s a -> s { _cevEngine = a })
{-# INLINE cevEngine #-}

-- | The version number of the cache engine.
cevEngineVersion :: Lens' CacheEngineVersion (Maybe Text)
cevEngineVersion = lens _cevEngineVersion (\s a -> s { _cevEngineVersion = a })
{-# INLINE cevEngineVersion #-}

-- | The name of the cache parameter group family associated with this cache
-- engine.
cevCacheParameterGroupFamily :: Lens' CacheEngineVersion (Maybe Text)
cevCacheParameterGroupFamily = lens _cevCacheParameterGroupFamily (\s a -> s { _cevCacheParameterGroupFamily = a })
{-# INLINE cevCacheParameterGroupFamily #-}

-- | The description of the cache engine.
cevCacheEngineDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineDescription = lens _cevCacheEngineDescription (\s a -> s { _cevCacheEngineDescription = a })
{-# INLINE cevCacheEngineDescription #-}

-- | The description of the cache engine version.
cevCacheEngineVersionDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineVersionDescription = lens _cevCacheEngineVersionDescription (\s a -> s { _cevCacheEngineVersionDescription = a })
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
cnCacheNodeId = lens _cnCacheNodeId (\s a -> s { _cnCacheNodeId = a })
{-# INLINE cnCacheNodeId #-}

-- | The current state of this cache node.
cnCacheNodeStatus :: Lens' CacheNode (Maybe Text)
cnCacheNodeStatus = lens _cnCacheNodeStatus (\s a -> s { _cnCacheNodeStatus = a })
{-# INLINE cnCacheNodeStatus #-}

-- | The date and time when the cache node was created.
cnCacheNodeCreateTime :: Lens' CacheNode (Maybe ISO8601)
cnCacheNodeCreateTime = lens _cnCacheNodeCreateTime (\s a -> s { _cnCacheNodeCreateTime = a })
{-# INLINE cnCacheNodeCreateTime #-}

-- | The hostname and IP address for connecting to this cache node.
cnEndpoint :: Lens' CacheNode (Maybe Endpoint)
cnEndpoint = lens _cnEndpoint (\s a -> s { _cnEndpoint = a })
{-# INLINE cnEndpoint #-}

-- | The status of the parameter group applied to this cache node.
cnParameterGroupStatus :: Lens' CacheNode (Maybe Text)
cnParameterGroupStatus = lens _cnParameterGroupStatus (\s a -> s { _cnParameterGroupStatus = a })
{-# INLINE cnParameterGroupStatus #-}

-- | The ID of the primary node to which this read replica node is synchronized.
-- If this field is empty, then this node is not associated with a primary
-- cache cluster.
cnSourceCacheNodeId :: Lens' CacheNode (Maybe Text)
cnSourceCacheNodeId = lens _cnSourceCacheNodeId (\s a -> s { _cnSourceCacheNodeId = a })
{-# INLINE cnSourceCacheNodeId #-}

-- | The Availability Zone where this node was created and now resides.
cnCustomerAvailabilityZone :: Lens' CacheNode (Maybe Text)
cnCustomerAvailabilityZone = lens _cnCustomerAvailabilityZone (\s a -> s { _cnCustomerAvailabilityZone = a })
{-# INLINE cnCustomerAvailabilityZone #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheNode' data type to populate a request.
mkCacheNode :: CacheNode
mkCacheNode = CacheNode
    { _cnCacheNodeId = Nothing
    , _cnCacheNodeStatus = Nothing
    , _cnCacheNodeCreateTime = Nothing
    , _cnEndpoint = Nothing
    , _cnParameterGroupStatus = Nothing
    , _cnSourceCacheNodeId = Nothing
    , _cnCustomerAvailabilityZone = Nothing
    }
{-# INLINE mkCacheNode #-}

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
cntspParameterName = lens _cntspParameterName (\s a -> s { _cntspParameterName = a })
{-# INLINE cntspParameterName #-}

-- | A description of the parameter.
cntspDescription :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDescription = lens _cntspDescription (\s a -> s { _cntspDescription = a })
{-# INLINE cntspDescription #-}

-- | The source of the parameter value.
cntspSource :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspSource = lens _cntspSource (\s a -> s { _cntspSource = a })
{-# INLINE cntspSource #-}

-- | The valid data type for the parameter.
cntspDataType :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDataType = lens _cntspDataType (\s a -> s { _cntspDataType = a })
{-# INLINE cntspDataType #-}

-- | The valid range of values for the parameter.
cntspAllowedValues :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspAllowedValues = lens _cntspAllowedValues (\s a -> s { _cntspAllowedValues = a })
{-# INLINE cntspAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
cntspIsModifiable :: Lens' CacheNodeTypeSpecificParameter (Maybe Bool)
cntspIsModifiable = lens _cntspIsModifiable (\s a -> s { _cntspIsModifiable = a })
{-# INLINE cntspIsModifiable #-}

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspMinimumEngineVersion = lens _cntspMinimumEngineVersion (\s a -> s { _cntspMinimumEngineVersion = a })
{-# INLINE cntspMinimumEngineVersion #-}

-- | A list of cache node types and their corresponding values for this
-- parameter.
cntspCacheNodeTypeSpecificValues :: Lens' CacheNodeTypeSpecificParameter ([CacheNodeTypeSpecificValue])
cntspCacheNodeTypeSpecificValues = lens _cntspCacheNodeTypeSpecificValues (\s a -> s { _cntspCacheNodeTypeSpecificValues = a })
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
cntsvCacheNodeType = lens _cntsvCacheNodeType (\s a -> s { _cntsvCacheNodeType = a })
{-# INLINE cntsvCacheNodeType #-}

-- | The value for the cache node type.
cntsvValue :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvValue = lens _cntsvValue (\s a -> s { _cntsvValue = a })
{-# INLINE cntsvValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheNodeTypeSpecificValue' data type to populate a request.
mkCacheNodeTypeSpecificValue :: CacheNodeTypeSpecificValue
mkCacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { _cntsvCacheNodeType = Nothing
    , _cntsvValue = Nothing
    }
{-# INLINE mkCacheNodeTypeSpecificValue #-}

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
cpgCacheParameterGroupName = lens _cpgCacheParameterGroupName (\s a -> s { _cpgCacheParameterGroupName = a })
{-# INLINE cpgCacheParameterGroupName #-}

-- | The name of the cache parameter group family that this cache parameter
-- group is compatible with.
cpgCacheParameterGroupFamily :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupFamily = lens _cpgCacheParameterGroupFamily (\s a -> s { _cpgCacheParameterGroupFamily = a })
{-# INLINE cpgCacheParameterGroupFamily #-}

-- | The description for this cache parameter group.
cpgDescription :: Lens' CacheParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\s a -> s { _cpgDescription = a })
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
cpgsCacheParameterGroupName = lens _cpgsCacheParameterGroupName (\s a -> s { _cpgsCacheParameterGroupName = a })
{-# INLINE cpgsCacheParameterGroupName #-}

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus = lens _cpgsParameterApplyStatus (\s a -> s { _cpgsParameterApplyStatus = a })
{-# INLINE cpgsParameterApplyStatus #-}

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cpgsCacheNodeIdsToReboot :: Lens' CacheParameterGroupStatus ([Text])
cpgsCacheNodeIdsToReboot = lens _cpgsCacheNodeIdsToReboot (\s a -> s { _cpgsCacheNodeIdsToReboot = a })
{-# INLINE cpgsCacheNodeIdsToReboot #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheParameterGroupStatus' data type to populate a request.
mkCacheParameterGroupStatus :: CacheParameterGroupStatus
mkCacheParameterGroupStatus = CacheParameterGroupStatus
    { _cpgsCacheParameterGroupName = Nothing
    , _cpgsParameterApplyStatus = Nothing
    , _cpgsCacheNodeIdsToReboot = mempty
    }
{-# INLINE mkCacheParameterGroupStatus #-}

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
csgOwnerId = lens _csgOwnerId (\s a -> s { _csgOwnerId = a })
{-# INLINE csgOwnerId #-}

-- | The name of the cache security group.
csgCacheSecurityGroupName :: Lens' CacheSecurityGroup (Maybe Text)
csgCacheSecurityGroupName = lens _csgCacheSecurityGroupName (\s a -> s { _csgCacheSecurityGroupName = a })
{-# INLINE csgCacheSecurityGroupName #-}

-- | The description of the cache security group.
csgDescription :: Lens' CacheSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })
{-# INLINE csgDescription #-}

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
csgEC2SecurityGroups :: Lens' CacheSecurityGroup ([EC2SecurityGroup])
csgEC2SecurityGroups = lens _csgEC2SecurityGroups (\s a -> s { _csgEC2SecurityGroups = a })
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
csgmCacheSecurityGroupName = lens _csgmCacheSecurityGroupName (\s a -> s { _csgmCacheSecurityGroupName = a })
{-# INLINE csgmCacheSecurityGroupName #-}

-- | The membership status in the cache security group. The status changes when
-- a cache security group is modified, or when the cache security groups
-- assigned to a cache cluster are modified.
csgmStatus :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s { _csgmStatus = a })
{-# INLINE csgmStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheSecurityGroupMembership' data type to populate a request.
mkCacheSecurityGroupMembership :: CacheSecurityGroupMembership
mkCacheSecurityGroupMembership = CacheSecurityGroupMembership
    { _csgmCacheSecurityGroupName = Nothing
    , _csgmStatus = Nothing
    }
{-# INLINE mkCacheSecurityGroupMembership #-}

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
csiCacheSubnetGroupName = lens _csiCacheSubnetGroupName (\s a -> s { _csiCacheSubnetGroupName = a })
{-# INLINE csiCacheSubnetGroupName #-}

-- | The description of the cache subnet group.
csiCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
csiCacheSubnetGroupDescription = lens _csiCacheSubnetGroupDescription (\s a -> s { _csiCacheSubnetGroupDescription = a })
{-# INLINE csiCacheSubnetGroupDescription #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
csiVpcId :: Lens' CacheSubnetGroup (Maybe Text)
csiVpcId = lens _csiVpcId (\s a -> s { _csiVpcId = a })
{-# INLINE csiVpcId #-}

-- | A list of subnets associated with the cache subnet group.
csiSubnets :: Lens' CacheSubnetGroup ([Subnet])
csiSubnets = lens _csiSubnets (\s a -> s { _csiSubnets = a })
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
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })
{-# INLINE ecsgStatus #-}

-- | The name of the Amazon EC2 security group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName = lens _ecsgEC2SecurityGroupName (\s a -> s { _ecsgEC2SecurityGroupName = a })
{-# INLINE ecsgEC2SecurityGroupName #-}

-- | The AWS account ID of the Amazon EC2 security group owner.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId = lens _ecsgEC2SecurityGroupOwnerId (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })
{-# INLINE ecsgEC2SecurityGroupOwnerId #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EC2SecurityGroup' data type to populate a request.
mkEC2SecurityGroup :: EC2SecurityGroup
mkEC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus = Nothing
    , _ecsgEC2SecurityGroupName = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }
{-# INLINE mkEC2SecurityGroup #-}

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
eAddress = lens _eAddress (\s a -> s { _eAddress = a })
{-# INLINE eAddress #-}

-- | The port number that the cache engine is listening on.
ePort :: Lens' Endpoint (Maybe Integer)
ePort = lens _ePort (\s a -> s { _ePort = a })
{-# INLINE ePort #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint' data type to populate a request.
mkEndpoint :: Endpoint
mkEndpoint = Endpoint
    { _eAddress = Nothing
    , _ePort = Nothing
    }
{-# INLINE mkEndpoint #-}

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
edCacheParameterGroupFamily = lens _edCacheParameterGroupFamily (\s a -> s { _edCacheParameterGroupFamily = a })
{-# INLINE edCacheParameterGroupFamily #-}

-- | Provides an identifier to allow retrieval of paginated results.
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\s a -> s { _edMarker = a })
{-# INLINE edMarker #-}

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults ([Parameter])
edParameters = lens _edParameters (\s a -> s { _edParameters = a })
{-# INLINE edParameters #-}

-- | A list of parameters specific to a particular cache node type. Each element
-- in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters :: Lens' EngineDefaults ([CacheNodeTypeSpecificParameter])
edCacheNodeTypeSpecificParameters = lens _edCacheNodeTypeSpecificParameters (\s a -> s { _edCacheNodeTypeSpecificParameters = a })
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
exSourceIdentifier = lens _exSourceIdentifier (\s a -> s { _exSourceIdentifier = a })
{-# INLINE exSourceIdentifier #-}

-- | Specifies the origin of this event - a cache cluster, a parameter group, a
-- security group, etc.
exSourceType :: Lens' Event (Maybe SourceType)
exSourceType = lens _exSourceType (\s a -> s { _exSourceType = a })
{-# INLINE exSourceType #-}

-- | The text of the event.
exMessage :: Lens' Event (Maybe Text)
exMessage = lens _exMessage (\s a -> s { _exMessage = a })
{-# INLINE exMessage #-}

-- | The date and time when the event occurred.
exDate :: Lens' Event (Maybe ISO8601)
exDate = lens _exDate (\s a -> s { _exDate = a })
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
ngNodeGroupId = lens _ngNodeGroupId (\s a -> s { _ngNodeGroupId = a })
{-# INLINE ngNodeGroupId #-}

-- | The current state of this replication group - creating, available, etc.
ngStatus :: Lens' NodeGroup (Maybe Text)
ngStatus = lens _ngStatus (\s a -> s { _ngStatus = a })
{-# INLINE ngStatus #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ngPrimaryEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngPrimaryEndpoint = lens _ngPrimaryEndpoint (\s a -> s { _ngPrimaryEndpoint = a })
{-# INLINE ngPrimaryEndpoint #-}

-- | A list containing information about individual nodes within the node group.
ngNodeGroupMembers :: Lens' NodeGroup ([NodeGroupMember])
ngNodeGroupMembers = lens _ngNodeGroupMembers (\s a -> s { _ngNodeGroupMembers = a })
{-# INLINE ngNodeGroupMembers #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NodeGroup' data type to populate a request.
mkNodeGroup :: NodeGroup
mkNodeGroup = NodeGroup
    { _ngNodeGroupId = Nothing
    , _ngStatus = Nothing
    , _ngPrimaryEndpoint = Nothing
    , _ngNodeGroupMembers = mempty
    }
{-# INLINE mkNodeGroup #-}

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
ngmCacheClusterId = lens _ngmCacheClusterId (\s a -> s { _ngmCacheClusterId = a })
{-# INLINE ngmCacheClusterId #-}

-- | The ID of the node within its cache cluster. A node ID is a numeric
-- identifier (0001, 0002, etc.).
ngmCacheNodeId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheNodeId = lens _ngmCacheNodeId (\s a -> s { _ngmCacheNodeId = a })
{-# INLINE ngmCacheNodeId #-}

-- | Represents the information required for client programs to connect to a
-- cache node.
ngmReadEndpoint :: Lens' NodeGroupMember (Maybe Endpoint)
ngmReadEndpoint = lens _ngmReadEndpoint (\s a -> s { _ngmReadEndpoint = a })
{-# INLINE ngmReadEndpoint #-}

-- | The name of the Availability Zone in which the node is located.
ngmPreferredAvailabilityZone :: Lens' NodeGroupMember (Maybe Text)
ngmPreferredAvailabilityZone = lens _ngmPreferredAvailabilityZone (\s a -> s { _ngmPreferredAvailabilityZone = a })
{-# INLINE ngmPreferredAvailabilityZone #-}

-- | The role that is currently assigned to the node - primary or replica.
ngmCurrentRole :: Lens' NodeGroupMember (Maybe Text)
ngmCurrentRole = lens _ngmCurrentRole (\s a -> s { _ngmCurrentRole = a })
{-# INLINE ngmCurrentRole #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NodeGroupMember' data type to populate a request.
mkNodeGroupMember :: NodeGroupMember
mkNodeGroupMember = NodeGroupMember
    { _ngmCacheClusterId = Nothing
    , _ngmCacheNodeId = Nothing
    , _ngmReadEndpoint = Nothing
    , _ngmPreferredAvailabilityZone = Nothing
    , _ngmCurrentRole = Nothing
    }
{-# INLINE mkNodeGroupMember #-}

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
nsCacheNodeId = lens _nsCacheNodeId (\s a -> s { _nsCacheNodeId = a })
{-# INLINE nsCacheNodeId #-}

-- | The size of the cache on the source cache node.
nsCacheSize :: Lens' NodeSnapshot (Maybe Text)
nsCacheSize = lens _nsCacheSize (\s a -> s { _nsCacheSize = a })
{-# INLINE nsCacheSize #-}

-- | The date and time when the cache node was created in the source cache
-- cluster.
nsCacheNodeCreateTime :: Lens' NodeSnapshot (Maybe ISO8601)
nsCacheNodeCreateTime = lens _nsCacheNodeCreateTime (\s a -> s { _nsCacheNodeCreateTime = a })
{-# INLINE nsCacheNodeCreateTime #-}

-- | The date and time when the source node's metadata and cache data set was
-- obtained for the snapshot.
nsSnapshotCreateTime :: Lens' NodeSnapshot (Maybe ISO8601)
nsSnapshotCreateTime = lens _nsSnapshotCreateTime (\s a -> s { _nsSnapshotCreateTime = a })
{-# INLINE nsSnapshotCreateTime #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NodeSnapshot' data type to populate a request.
mkNodeSnapshot :: NodeSnapshot
mkNodeSnapshot = NodeSnapshot
    { _nsCacheNodeId = Nothing
    , _nsCacheSize = Nothing
    , _nsCacheNodeCreateTime = Nothing
    , _nsSnapshotCreateTime = Nothing
    }
{-# INLINE mkNodeSnapshot #-}

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
ncTopicArn = lens _ncTopicArn (\s a -> s { _ncTopicArn = a })
{-# INLINE ncTopicArn #-}

-- | The current state of the topic.
ncTopicStatus :: Lens' NotificationConfiguration (Maybe Text)
ncTopicStatus = lens _ncTopicStatus (\s a -> s { _ncTopicStatus = a })
{-# INLINE ncTopicStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NotificationConfiguration' data type to populate a request.
mkNotificationConfiguration :: NotificationConfiguration
mkNotificationConfiguration = NotificationConfiguration
    { _ncTopicArn = Nothing
    , _ncTopicStatus = Nothing
    }
{-# INLINE mkNotificationConfiguration #-}

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
prParameterName = lens _prParameterName (\s a -> s { _prParameterName = a })
{-# INLINE prParameterName #-}

-- | The value of the parameter.
prParameterValue :: Lens' Parameter (Maybe Text)
prParameterValue = lens _prParameterValue (\s a -> s { _prParameterValue = a })
{-# INLINE prParameterValue #-}

-- | A description of the parameter.
prDescription :: Lens' Parameter (Maybe Text)
prDescription = lens _prDescription (\s a -> s { _prDescription = a })
{-# INLINE prDescription #-}

-- | The source of the parameter.
prSource :: Lens' Parameter (Maybe Text)
prSource = lens _prSource (\s a -> s { _prSource = a })
{-# INLINE prSource #-}

-- | The valid data type for the parameter.
prDataType :: Lens' Parameter (Maybe Text)
prDataType = lens _prDataType (\s a -> s { _prDataType = a })
{-# INLINE prDataType #-}

-- | The valid range of values for the parameter.
prAllowedValues :: Lens' Parameter (Maybe Text)
prAllowedValues = lens _prAllowedValues (\s a -> s { _prAllowedValues = a })
{-# INLINE prAllowedValues #-}

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
prIsModifiable :: Lens' Parameter (Maybe Bool)
prIsModifiable = lens _prIsModifiable (\s a -> s { _prIsModifiable = a })
{-# INLINE prIsModifiable #-}

-- | The earliest cache engine version to which the parameter can apply.
prMinimumEngineVersion :: Lens' Parameter (Maybe Text)
prMinimumEngineVersion = lens _prMinimumEngineVersion (\s a -> s { _prMinimumEngineVersion = a })
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
pnvParameterName = lens _pnvParameterName (\s a -> s { _pnvParameterName = a })
{-# INLINE pnvParameterName #-}

-- | The value of the parameter.
pnvParameterValue :: Lens' ParameterNameValue (Maybe Text)
pnvParameterValue = lens _pnvParameterValue (\s a -> s { _pnvParameterValue = a })
{-# INLINE pnvParameterValue #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ParameterNameValue' data type to populate a request.
mkParameterNameValue :: ParameterNameValue
mkParameterNameValue = ParameterNameValue
    { _pnvParameterName = Nothing
    , _pnvParameterValue = Nothing
    }
{-# INLINE mkParameterNameValue #-}

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
pmvNumCacheNodes = lens _pmvNumCacheNodes (\s a -> s { _pmvNumCacheNodes = a })
{-# INLINE pmvNumCacheNodes #-}

-- | A list of cache node IDs that are being removed (or will be removed) from
-- the cache cluster. A node ID is a numeric identifier (0001, 0002, etc.).
pmvCacheNodeIdsToRemove :: Lens' PendingModifiedValues ([Text])
pmvCacheNodeIdsToRemove = lens _pmvCacheNodeIdsToRemove (\s a -> s { _pmvCacheNodeIdsToRemove = a })
{-# INLINE pmvCacheNodeIdsToRemove #-}

-- | The new cache engine version that the cache cluster will run.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\s a -> s { _pmvEngineVersion = a })
{-# INLINE pmvEngineVersion #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PendingModifiedValues' data type to populate a request.
mkPendingModifiedValues :: PendingModifiedValues
mkPendingModifiedValues = PendingModifiedValues
    { _pmvNumCacheNodes = Nothing
    , _pmvCacheNodeIdsToRemove = mempty
    , _pmvEngineVersion = Nothing
    }
{-# INLINE mkPendingModifiedValues #-}

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
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\s a -> s { _rcRecurringChargeAmount = a })
{-# INLINE rcRecurringChargeAmount #-}

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\s a -> s { _rcRecurringChargeFrequency = a })
{-# INLINE rcRecurringChargeFrequency #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
mkRecurringCharge :: RecurringCharge
mkRecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }
{-# INLINE mkRecurringCharge #-}

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
rgReplicationGroupId = lens _rgReplicationGroupId (\s a -> s { _rgReplicationGroupId = a })
{-# INLINE rgReplicationGroupId #-}

-- | The description of the replication group.
rgDescription :: Lens' ReplicationGroup (Maybe Text)
rgDescription = lens _rgDescription (\s a -> s { _rgDescription = a })
{-# INLINE rgDescription #-}

-- | The current state of this replication group - creating, available, etc.
rgStatus :: Lens' ReplicationGroup (Maybe Text)
rgStatus = lens _rgStatus (\s a -> s { _rgStatus = a })
{-# INLINE rgStatus #-}

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
rgPendingModifiedValues :: Lens' ReplicationGroup (Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues = lens _rgPendingModifiedValues (\s a -> s { _rgPendingModifiedValues = a })
{-# INLINE rgPendingModifiedValues #-}

-- | The names of all the cache clusters that are part of this replication
-- group.
rgMemberClusters :: Lens' ReplicationGroup ([Text])
rgMemberClusters = lens _rgMemberClusters (\s a -> s { _rgMemberClusters = a })
{-# INLINE rgMemberClusters #-}

-- | A single element list with information about the nodes in the replication
-- group.
rgNodeGroups :: Lens' ReplicationGroup ([NodeGroup])
rgNodeGroups = lens _rgNodeGroups (\s a -> s { _rgNodeGroups = a })
{-# INLINE rgNodeGroups #-}

-- | The cache cluster ID that is used as the daily snapshot source for the
-- replication group.
rgSnapshottingClusterId :: Lens' ReplicationGroup (Maybe Text)
rgSnapshottingClusterId = lens _rgSnapshottingClusterId (\s a -> s { _rgSnapshottingClusterId = a })
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
rcnReservedCacheNodeId = lens _rcnReservedCacheNodeId (\s a -> s { _rcnReservedCacheNodeId = a })
{-# INLINE rcnReservedCacheNodeId #-}

-- | The offering identifier.
rcnReservedCacheNodesOfferingId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodesOfferingId = lens _rcnReservedCacheNodesOfferingId (\s a -> s { _rcnReservedCacheNodesOfferingId = a })
{-# INLINE rcnReservedCacheNodesOfferingId #-}

-- | The cache node type for the reserved cache nodes.
rcnCacheNodeType :: Lens' ReservedCacheNode (Maybe Text)
rcnCacheNodeType = lens _rcnCacheNodeType (\s a -> s { _rcnCacheNodeType = a })
{-# INLINE rcnCacheNodeType #-}

-- | The time the reservation started.
rcnStartTime :: Lens' ReservedCacheNode (Maybe ISO8601)
rcnStartTime = lens _rcnStartTime (\s a -> s { _rcnStartTime = a })
{-# INLINE rcnStartTime #-}

-- | The duration of the reservation in seconds.
rcnDuration :: Lens' ReservedCacheNode (Maybe Integer)
rcnDuration = lens _rcnDuration (\s a -> s { _rcnDuration = a })
{-# INLINE rcnDuration #-}

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice :: Lens' ReservedCacheNode (Maybe Double)
rcnFixedPrice = lens _rcnFixedPrice (\s a -> s { _rcnFixedPrice = a })
{-# INLINE rcnFixedPrice #-}

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice :: Lens' ReservedCacheNode (Maybe Double)
rcnUsagePrice = lens _rcnUsagePrice (\s a -> s { _rcnUsagePrice = a })
{-# INLINE rcnUsagePrice #-}

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount :: Lens' ReservedCacheNode (Maybe Integer)
rcnCacheNodeCount = lens _rcnCacheNodeCount (\s a -> s { _rcnCacheNodeCount = a })
{-# INLINE rcnCacheNodeCount #-}

-- | The description of the reserved cache node.
rcnProductDescription :: Lens' ReservedCacheNode (Maybe Text)
rcnProductDescription = lens _rcnProductDescription (\s a -> s { _rcnProductDescription = a })
{-# INLINE rcnProductDescription #-}

-- | The offering type of this reserved cache node.
rcnOfferingType :: Lens' ReservedCacheNode (Maybe Text)
rcnOfferingType = lens _rcnOfferingType (\s a -> s { _rcnOfferingType = a })
{-# INLINE rcnOfferingType #-}

-- | The state of the reserved cache node.
rcnState :: Lens' ReservedCacheNode (Maybe Text)
rcnState = lens _rcnState (\s a -> s { _rcnState = a })
{-# INLINE rcnState #-}

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges :: Lens' ReservedCacheNode ([RecurringCharge])
rcnRecurringCharges = lens _rcnRecurringCharges (\s a -> s { _rcnRecurringCharges = a })
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
rcnoReservedCacheNodesOfferingId = lens _rcnoReservedCacheNodesOfferingId (\s a -> s { _rcnoReservedCacheNodesOfferingId = a })
{-# INLINE rcnoReservedCacheNodesOfferingId #-}

-- | The cache node type for the reserved cache node.
rcnoCacheNodeType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoCacheNodeType = lens _rcnoCacheNodeType (\s a -> s { _rcnoCacheNodeType = a })
{-# INLINE rcnoCacheNodeType #-}

-- | The duration of the offering. in seconds.
rcnoDuration :: Lens' ReservedCacheNodesOffering (Maybe Integer)
rcnoDuration = lens _rcnoDuration (\s a -> s { _rcnoDuration = a })
{-# INLINE rcnoDuration #-}

-- | The fixed price charged for this offering.
rcnoFixedPrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoFixedPrice = lens _rcnoFixedPrice (\s a -> s { _rcnoFixedPrice = a })
{-# INLINE rcnoFixedPrice #-}

-- | The hourly price charged for this offering.
rcnoUsagePrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoUsagePrice = lens _rcnoUsagePrice (\s a -> s { _rcnoUsagePrice = a })
{-# INLINE rcnoUsagePrice #-}

-- | The cache engine used by the offering.
rcnoProductDescription :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoProductDescription = lens _rcnoProductDescription (\s a -> s { _rcnoProductDescription = a })
{-# INLINE rcnoProductDescription #-}

-- | The offering type.
rcnoOfferingType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoOfferingType = lens _rcnoOfferingType (\s a -> s { _rcnoOfferingType = a })
{-# INLINE rcnoOfferingType #-}

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges :: Lens' ReservedCacheNodesOffering ([RecurringCharge])
rcnoRecurringCharges = lens _rcnoRecurringCharges (\s a -> s { _rcnoRecurringCharges = a })
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
sgmSecurityGroupId = lens _sgmSecurityGroupId (\s a -> s { _sgmSecurityGroupId = a })
{-# INLINE sgmSecurityGroupId #-}

-- | The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cache cluster are modified.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus = lens _sgmStatus (\s a -> s { _sgmStatus = a })
{-# INLINE sgmStatus #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SecurityGroupMembership' data type to populate a request.
mkSecurityGroupMembership :: SecurityGroupMembership
mkSecurityGroupMembership = SecurityGroupMembership
    { _sgmSecurityGroupId = Nothing
    , _sgmStatus = Nothing
    }
{-# INLINE mkSecurityGroupMembership #-}

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
stSnapshotName = lens _stSnapshotName (\s a -> s { _stSnapshotName = a })
{-# INLINE stSnapshotName #-}

-- | The user-supplied identifier of the source cache cluster.
stCacheClusterId :: Lens' Snapshot (Maybe Text)
stCacheClusterId = lens _stCacheClusterId (\s a -> s { _stCacheClusterId = a })
{-# INLINE stCacheClusterId #-}

-- | The status of the snapshot. Valid values: creating | available | restoring
-- | copying | deleting.
stSnapshotStatus :: Lens' Snapshot (Maybe Text)
stSnapshotStatus = lens _stSnapshotStatus (\s a -> s { _stSnapshotStatus = a })
{-# INLINE stSnapshotStatus #-}

-- | Indicates whether the snapshot is from an automatic backup (automated) or
-- was created manually (manual).
stSnapshotSource :: Lens' Snapshot (Maybe Text)
stSnapshotSource = lens _stSnapshotSource (\s a -> s { _stSnapshotSource = a })
{-# INLINE stSnapshotSource #-}

-- | The name of the compute and memory capacity node type for the source cache
-- cluster.
stCacheNodeType :: Lens' Snapshot (Maybe Text)
stCacheNodeType = lens _stCacheNodeType (\s a -> s { _stCacheNodeType = a })
{-# INLINE stCacheNodeType #-}

-- | The name of the cache engine (memcached or redis) used by the source cache
-- cluster.
stEngine :: Lens' Snapshot (Maybe Text)
stEngine = lens _stEngine (\s a -> s { _stEngine = a })
{-# INLINE stEngine #-}

-- | The version of the cache engine version that is used by the source cache
-- cluster.
stEngineVersion :: Lens' Snapshot (Maybe Text)
stEngineVersion = lens _stEngineVersion (\s a -> s { _stEngineVersion = a })
{-# INLINE stEngineVersion #-}

-- | The number of cache nodes in the source cache cluster.
stNumCacheNodes :: Lens' Snapshot (Maybe Integer)
stNumCacheNodes = lens _stNumCacheNodes (\s a -> s { _stNumCacheNodes = a })
{-# INLINE stNumCacheNodes #-}

-- | The name of the Availability Zone in which the source cache cluster is
-- located.
stPreferredAvailabilityZone :: Lens' Snapshot (Maybe Text)
stPreferredAvailabilityZone = lens _stPreferredAvailabilityZone (\s a -> s { _stPreferredAvailabilityZone = a })
{-# INLINE stPreferredAvailabilityZone #-}

-- | The date and time when the source cache cluster was created.
stCacheClusterCreateTime :: Lens' Snapshot (Maybe ISO8601)
stCacheClusterCreateTime = lens _stCacheClusterCreateTime (\s a -> s { _stCacheClusterCreateTime = a })
{-# INLINE stCacheClusterCreateTime #-}

-- | The time range (in UTC) during which weekly system maintenance can occur on
-- the source cache cluster.
stPreferredMaintenanceWindow :: Lens' Snapshot (Maybe Text)
stPreferredMaintenanceWindow = lens _stPreferredMaintenanceWindow (\s a -> s { _stPreferredMaintenanceWindow = a })
{-# INLINE stPreferredMaintenanceWindow #-}

-- | The Amazon Resource Name (ARN) for the topic used by the source cache
-- cluster for publishing notifications.
stTopicArn :: Lens' Snapshot (Maybe Text)
stTopicArn = lens _stTopicArn (\s a -> s { _stTopicArn = a })
{-# INLINE stTopicArn #-}

-- | The port number used by each cache nodes in the source cache cluster.
stPort :: Lens' Snapshot (Maybe Integer)
stPort = lens _stPort (\s a -> s { _stPort = a })
{-# INLINE stPort #-}

-- | The cache parameter group that is associated with the source cache cluster.
stCacheParameterGroupName :: Lens' Snapshot (Maybe Text)
stCacheParameterGroupName = lens _stCacheParameterGroupName (\s a -> s { _stCacheParameterGroupName = a })
{-# INLINE stCacheParameterGroupName #-}

-- | The name of the cache subnet group associated with the source cache
-- cluster.
stCacheSubnetGroupName :: Lens' Snapshot (Maybe Text)
stCacheSubnetGroupName = lens _stCacheSubnetGroupName (\s a -> s { _stCacheSubnetGroupName = a })
{-# INLINE stCacheSubnetGroupName #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cache cluster.
stVpcId :: Lens' Snapshot (Maybe Text)
stVpcId = lens _stVpcId (\s a -> s { _stVpcId = a })
{-# INLINE stVpcId #-}

-- | For the source cache cluster, indicates whether minor version patches are
-- applied automatically (true) or not (false).
stAutoMinorVersionUpgrade :: Lens' Snapshot (Maybe Bool)
stAutoMinorVersionUpgrade = lens _stAutoMinorVersionUpgrade (\s a -> s { _stAutoMinorVersionUpgrade = a })
{-# INLINE stAutoMinorVersionUpgrade #-}

-- | For an automatic snapshot, the number of days for which ElastiCache will
-- retain the snapshot before deleting it. For manual snapshots, this field
-- reflects the SnapshotRetentionLimit for the source cache cluster when the
-- snapshot was created. This field is otherwise ignored: Manual snapshots do
-- not expire, and can only be deleted using the DeleteSnapshot action.
-- ImportantIf the value of SnapshotRetentionLimit is set to zero (0), backups
-- are turned off.
stSnapshotRetentionLimit :: Lens' Snapshot (Maybe Integer)
stSnapshotRetentionLimit = lens _stSnapshotRetentionLimit (\s a -> s { _stSnapshotRetentionLimit = a })
{-# INLINE stSnapshotRetentionLimit #-}

-- | The daily time range during which ElastiCache takes daily snapshots of the
-- source cache cluster.
stSnapshotWindow :: Lens' Snapshot (Maybe Text)
stSnapshotWindow = lens _stSnapshotWindow (\s a -> s { _stSnapshotWindow = a })
{-# INLINE stSnapshotWindow #-}

-- | A list of the cache nodes in the source cache cluster.
stNodeSnapshots :: Lens' Snapshot ([NodeSnapshot])
stNodeSnapshots = lens _stNodeSnapshots (\s a -> s { _stNodeSnapshots = a })
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
sssssuSubnetIdentifier = lens _sssssuSubnetIdentifier (\s a -> s { _sssssuSubnetIdentifier = a })
{-# INLINE sssssuSubnetIdentifier #-}

-- | The Availability Zone associated with the subnet.
sssssuSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sssssuSubnetAvailabilityZone = lens _sssssuSubnetAvailabilityZone (\s a -> s { _sssssuSubnetAvailabilityZone = a })
{-# INLINE sssssuSubnetAvailabilityZone #-}

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type to populate a request.
mkSubnet :: Subnet
mkSubnet = Subnet
    { _sssssuSubnetIdentifier = Nothing
    , _sssssuSubnetAvailabilityZone = Nothing
    }
{-# INLINE mkSubnet #-}

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def
