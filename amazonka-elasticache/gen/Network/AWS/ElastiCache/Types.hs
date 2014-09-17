{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.Types
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
module Network.AWS.ElastiCache.Types
    (
    -- * Service
      ElastiCache
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
    , mkCacheCluster
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
    , mkCacheEngineVersion
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
    , mkCacheNodeTypeSpecificParameter
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
    , mkCacheParameterGroup
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
    , mkCacheSecurityGroup
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
    , mkCacheSubnetGroup
    , csgrCacheSubnetGroupName
    , csgrCacheSubnetGroupDescription
    , csgrVpcId
    , csgrSubnets

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
    , mkEngineDefaults
    , edCacheParameterGroupFamily
    , edMarker
    , edParameters
    , edCacheNodeTypeSpecificParameters

    -- * Event
    , Event
    , mkEvent
    , erSourceIdentifier
    , erSourceType
    , erMessage
    , erDate

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
    , mkParameter
    , pParameterName
    , pParameterValue
    , pDescription
    , pSource
    , pDataType
    , pAllowedValues
    , pIsModifiable
    , pMinimumEngineVersion

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
    , mkReplicationGroup
    , rgReplicationGroupId
    , rgDescription
    , rgStatus
    , rgPendingModifiedValues
    , rgMemberClusters
    , rgNodeGroups
    , rgSnapshottingClusterId

    -- * ReservedCacheNode
    , ReservedCacheNode
    , mkReservedCacheNode
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
    , mkReservedCacheNodesOffering
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
    , mkSnapshot
    , sSnapshotName
    , sCacheClusterId
    , sSnapshotStatus
    , sSnapshotSource
    , sCacheNodeType
    , sEngine
    , sEngineVersion
    , sNumCacheNodes
    , sPreferredAvailabilityZone
    , sCacheClusterCreateTime
    , sPreferredMaintenanceWindow
    , sTopicArn
    , sPort
    , sCacheParameterGroupName
    , sCacheSubnetGroupName
    , sVpcId
    , sAutoMinorVersionUpgrade
    , sSnapshotRetentionLimit
    , sSnapshotWindow
    , sNodeSnapshots

    -- * Subnet
    , Subnet
    , mkSubnet
    , srSubnetIdentifier
    , srSubnetAvailabilityZone
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
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'AvailabilityZone' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Maybe Text@
--
mkAvailabilityZone :: AvailabilityZone
mkAvailabilityZone = AvailabilityZone
    { _azName = Nothing
    }

-- | The name of the Availability Zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s { _azName = a })

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericQuery def

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
newtype ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { _rgpmvPrimaryClusterId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReplicationGroupPendingModifiedValues' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @PrimaryClusterId ::@ @Maybe Text@
--
mkReplicationGroupPendingModifiedValues :: ReplicationGroupPendingModifiedValues
mkReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { _rgpmvPrimaryClusterId = Nothing
    }

-- | The primary cluster ID which will be applied immediately (if
-- --apply-immediately was specified), or during the next maintenance window.
rgpmvPrimaryClusterId :: Lens' ReplicationGroupPendingModifiedValues (Maybe Text)
rgpmvPrimaryClusterId =
    lens _rgpmvPrimaryClusterId (\s a -> s { _rgpmvPrimaryClusterId = a })

instance FromXML ReplicationGroupPendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroupPendingModifiedValues"

instance ToQuery ReplicationGroupPendingModifiedValues where
    toQuery = genericQuery def

-- | Contains all of the attributes of a specific cache cluster.
data CacheCluster = CacheCluster
    { _ccCacheClusterId :: Maybe Text
    , _ccConfigurationEndpoint :: Maybe Endpoint
    , _ccClientDownloadLandingPage :: Maybe Text
    , _ccCacheNodeType :: Maybe Text
    , _ccEngine :: Maybe Text
    , _ccEngineVersion :: Maybe Text
    , _ccCacheClusterStatus :: Maybe Text
    , _ccNumCacheNodes :: Maybe Integer
    , _ccPreferredAvailabilityZone :: Maybe Text
    , _ccCacheClusterCreateTime :: Maybe ISO8601
    , _ccPreferredMaintenanceWindow :: Maybe Text
    , _ccPendingModifiedValues :: Maybe PendingModifiedValues
    , _ccNotificationConfiguration :: Maybe NotificationConfiguration
    , _ccCacheSecurityGroups :: [CacheSecurityGroupMembership]
    , _ccCacheParameterGroup :: Maybe CacheParameterGroupStatus
    , _ccCacheSubnetGroupName :: Maybe Text
    , _ccCacheNodes :: [CacheNode]
    , _ccAutoMinorVersionUpgrade :: Maybe Bool
    , _ccSecurityGroups :: [SecurityGroupMembership]
    , _ccReplicationGroupId :: Maybe Text
    , _ccSnapshotRetentionLimit :: Maybe Integer
    , _ccSnapshotWindow :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheCluster' data type.
--
-- 'CacheCluster' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheClusterId ::@ @Maybe Text@
--
-- * @ConfigurationEndpoint ::@ @Maybe Endpoint@
--
-- * @ClientDownloadLandingPage ::@ @Maybe Text@
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @Engine ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @CacheClusterStatus ::@ @Maybe Text@
--
-- * @NumCacheNodes ::@ @Maybe Integer@
--
-- * @PreferredAvailabilityZone ::@ @Maybe Text@
--
-- * @CacheClusterCreateTime ::@ @Maybe ISO8601@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @PendingModifiedValues ::@ @Maybe PendingModifiedValues@
--
-- * @NotificationConfiguration ::@ @Maybe NotificationConfiguration@
--
-- * @CacheSecurityGroups ::@ @[CacheSecurityGroupMembership]@
--
-- * @CacheParameterGroup ::@ @Maybe CacheParameterGroupStatus@
--
-- * @CacheSubnetGroupName ::@ @Maybe Text@
--
-- * @CacheNodes ::@ @[CacheNode]@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @SecurityGroups ::@ @[SecurityGroupMembership]@
--
-- * @ReplicationGroupId ::@ @Maybe Text@
--
-- * @SnapshotRetentionLimit ::@ @Maybe Integer@
--
-- * @SnapshotWindow ::@ @Maybe Text@
--
mkCacheCluster :: CacheCluster
mkCacheCluster = CacheCluster
    { _ccCacheClusterId = Nothing
    , _ccConfigurationEndpoint = Nothing
    , _ccClientDownloadLandingPage = Nothing
    , _ccCacheNodeType = Nothing
    , _ccEngine = Nothing
    , _ccEngineVersion = Nothing
    , _ccCacheClusterStatus = Nothing
    , _ccNumCacheNodes = Nothing
    , _ccPreferredAvailabilityZone = Nothing
    , _ccCacheClusterCreateTime = Nothing
    , _ccPreferredMaintenanceWindow = Nothing
    , _ccPendingModifiedValues = Nothing
    , _ccNotificationConfiguration = Nothing
    , _ccCacheSecurityGroups = mempty
    , _ccCacheParameterGroup = Nothing
    , _ccCacheSubnetGroupName = Nothing
    , _ccCacheNodes = mempty
    , _ccAutoMinorVersionUpgrade = Nothing
    , _ccSecurityGroups = mempty
    , _ccReplicationGroupId = Nothing
    , _ccSnapshotRetentionLimit = Nothing
    , _ccSnapshotWindow = Nothing
    }

-- | The user-supplied identifier of the cache cluster. This is a unique key
-- that identifies a cache cluster.
ccCacheClusterId :: Lens' CacheCluster (Maybe Text)
ccCacheClusterId =
    lens _ccCacheClusterId (\s a -> s { _ccCacheClusterId = a })

-- | Represents the information required for client programs to connect to a
-- cache node.
ccConfigurationEndpoint :: Lens' CacheCluster (Maybe Endpoint)
ccConfigurationEndpoint =
    lens _ccConfigurationEndpoint
         (\s a -> s { _ccConfigurationEndpoint = a })

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
ccClientDownloadLandingPage :: Lens' CacheCluster (Maybe Text)
ccClientDownloadLandingPage =
    lens _ccClientDownloadLandingPage
         (\s a -> s { _ccClientDownloadLandingPage = a })

-- | The name of the compute and memory capacity node type for the cache
-- cluster.
ccCacheNodeType :: Lens' CacheCluster (Maybe Text)
ccCacheNodeType = lens _ccCacheNodeType (\s a -> s { _ccCacheNodeType = a })

-- | The name of the cache engine (memcached or redis) to be used for this cache
-- cluster.
ccEngine :: Lens' CacheCluster (Maybe Text)
ccEngine = lens _ccEngine (\s a -> s { _ccEngine = a })

-- | The version of the cache engine version that is used in this cache cluster.
ccEngineVersion :: Lens' CacheCluster (Maybe Text)
ccEngineVersion = lens _ccEngineVersion (\s a -> s { _ccEngineVersion = a })

-- | The current state of this cache cluster - creating, available, etc.
ccCacheClusterStatus :: Lens' CacheCluster (Maybe Text)
ccCacheClusterStatus =
    lens _ccCacheClusterStatus (\s a -> s { _ccCacheClusterStatus = a })

-- | The number of cache nodes in the cache cluster.
ccNumCacheNodes :: Lens' CacheCluster (Maybe Integer)
ccNumCacheNodes = lens _ccNumCacheNodes (\s a -> s { _ccNumCacheNodes = a })

-- | The name of the Availability Zone in which the cache cluster is located or
-- "Multiple" if the cache nodes are located in different Availability Zones.
ccPreferredAvailabilityZone :: Lens' CacheCluster (Maybe Text)
ccPreferredAvailabilityZone =
    lens _ccPreferredAvailabilityZone
         (\s a -> s { _ccPreferredAvailabilityZone = a })

-- | The date and time when the cache cluster was created.
ccCacheClusterCreateTime :: Lens' CacheCluster (Maybe ISO8601)
ccCacheClusterCreateTime =
    lens _ccCacheClusterCreateTime
         (\s a -> s { _ccCacheClusterCreateTime = a })

-- | The time range (in UTC) during which weekly system maintenance can occur.
ccPreferredMaintenanceWindow :: Lens' CacheCluster (Maybe Text)
ccPreferredMaintenanceWindow =
    lens _ccPreferredMaintenanceWindow
         (\s a -> s { _ccPreferredMaintenanceWindow = a })

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
ccPendingModifiedValues :: Lens' CacheCluster (Maybe PendingModifiedValues)
ccPendingModifiedValues =
    lens _ccPendingModifiedValues
         (\s a -> s { _ccPendingModifiedValues = a })

-- | Describes a notification topic and its status. Notification topics are used
-- for publishing ElastiCache events to subscribers using Amazon Simple
-- Notification Service (SNS).
ccNotificationConfiguration :: Lens' CacheCluster (Maybe NotificationConfiguration)
ccNotificationConfiguration =
    lens _ccNotificationConfiguration
         (\s a -> s { _ccNotificationConfiguration = a })

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
ccCacheSecurityGroups :: Lens' CacheCluster [CacheSecurityGroupMembership]
ccCacheSecurityGroups =
    lens _ccCacheSecurityGroups (\s a -> s { _ccCacheSecurityGroups = a })

-- | The status of the cache parameter group.
ccCacheParameterGroup :: Lens' CacheCluster (Maybe CacheParameterGroupStatus)
ccCacheParameterGroup =
    lens _ccCacheParameterGroup (\s a -> s { _ccCacheParameterGroup = a })

-- | The name of the cache subnet group associated with the cache cluster.
ccCacheSubnetGroupName :: Lens' CacheCluster (Maybe Text)
ccCacheSubnetGroupName =
    lens _ccCacheSubnetGroupName (\s a -> s { _ccCacheSubnetGroupName = a })

-- | A list of cache nodes that are members of the cache cluster.
ccCacheNodes :: Lens' CacheCluster [CacheNode]
ccCacheNodes = lens _ccCacheNodes (\s a -> s { _ccCacheNodes = a })

-- | If true, then minor version patches are applied automatically; if false,
-- then automatic minor version patches are disabled.
ccAutoMinorVersionUpgrade :: Lens' CacheCluster (Maybe Bool)
ccAutoMinorVersionUpgrade =
    lens _ccAutoMinorVersionUpgrade
         (\s a -> s { _ccAutoMinorVersionUpgrade = a })

-- | A list of VPC Security Groups associated with the cache cluster.
ccSecurityGroups :: Lens' CacheCluster [SecurityGroupMembership]
ccSecurityGroups =
    lens _ccSecurityGroups (\s a -> s { _ccSecurityGroups = a })

-- | The replication group to which this cache cluster belongs. If this field is
-- empty, the cache cluster is not associated with any replication group.
ccReplicationGroupId :: Lens' CacheCluster (Maybe Text)
ccReplicationGroupId =
    lens _ccReplicationGroupId (\s a -> s { _ccReplicationGroupId = a })

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
ccSnapshotRetentionLimit :: Lens' CacheCluster (Maybe Integer)
ccSnapshotRetentionLimit =
    lens _ccSnapshotRetentionLimit
         (\s a -> s { _ccSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster. Example: 05:00-09:00.
ccSnapshotWindow :: Lens' CacheCluster (Maybe Text)
ccSnapshotWindow =
    lens _ccSnapshotWindow (\s a -> s { _ccSnapshotWindow = a })

instance FromXML CacheCluster where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheCluster"

-- | Provides all of the details about a particular cache engine version.
data CacheEngineVersion = CacheEngineVersion
    { _cevEngine :: Maybe Text
    , _cevEngineVersion :: Maybe Text
    , _cevCacheParameterGroupFamily :: Maybe Text
    , _cevCacheEngineDescription :: Maybe Text
    , _cevCacheEngineVersionDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheEngineVersion' data type.
--
-- 'CacheEngineVersion' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Engine ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @CacheParameterGroupFamily ::@ @Maybe Text@
--
-- * @CacheEngineDescription ::@ @Maybe Text@
--
-- * @CacheEngineVersionDescription ::@ @Maybe Text@
--
mkCacheEngineVersion :: CacheEngineVersion
mkCacheEngineVersion = CacheEngineVersion
    { _cevEngine = Nothing
    , _cevEngineVersion = Nothing
    , _cevCacheParameterGroupFamily = Nothing
    , _cevCacheEngineDescription = Nothing
    , _cevCacheEngineVersionDescription = Nothing
    }

-- | The name of the cache engine.
cevEngine :: Lens' CacheEngineVersion (Maybe Text)
cevEngine = lens _cevEngine (\s a -> s { _cevEngine = a })

-- | The version number of the cache engine.
cevEngineVersion :: Lens' CacheEngineVersion (Maybe Text)
cevEngineVersion =
    lens _cevEngineVersion (\s a -> s { _cevEngineVersion = a })

-- | The name of the cache parameter group family associated with this cache
-- engine.
cevCacheParameterGroupFamily :: Lens' CacheEngineVersion (Maybe Text)
cevCacheParameterGroupFamily =
    lens _cevCacheParameterGroupFamily
         (\s a -> s { _cevCacheParameterGroupFamily = a })

-- | The description of the cache engine.
cevCacheEngineDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineDescription =
    lens _cevCacheEngineDescription
         (\s a -> s { _cevCacheEngineDescription = a })

-- | The description of the cache engine version.
cevCacheEngineVersionDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineVersionDescription =
    lens _cevCacheEngineVersionDescription
         (\s a -> s { _cevCacheEngineVersionDescription = a })

instance FromXML CacheEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheEngineVersion"

-- | Represents an individual cache node within a cache cluster. Each cache node
-- runs its own instance of the cluster's protocol-compliant caching software
-- - either Memcached or Redis.
data CacheNode = CacheNode
    { _cnCacheNodeId :: Maybe Text
    , _cnCacheNodeStatus :: Maybe Text
    , _cnCacheNodeCreateTime :: Maybe ISO8601
    , _cnEndpoint :: Maybe Endpoint
    , _cnParameterGroupStatus :: Maybe Text
    , _cnSourceCacheNodeId :: Maybe Text
    , _cnCustomerAvailabilityZone :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheNode' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheNodeId ::@ @Maybe Text@
--
-- * @CacheNodeStatus ::@ @Maybe Text@
--
-- * @CacheNodeCreateTime ::@ @Maybe ISO8601@
--
-- * @Endpoint ::@ @Maybe Endpoint@
--
-- * @ParameterGroupStatus ::@ @Maybe Text@
--
-- * @SourceCacheNodeId ::@ @Maybe Text@
--
-- * @CustomerAvailabilityZone ::@ @Maybe Text@
--
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

-- | The cache node identifier. A node ID is a numeric identifier (0001, 0002,
-- etc.). The combination of cluster ID and node ID uniquely identifies every
-- cache node used in a customer's AWS account.
cnCacheNodeId :: Lens' CacheNode (Maybe Text)
cnCacheNodeId = lens _cnCacheNodeId (\s a -> s { _cnCacheNodeId = a })

-- | The current state of this cache node.
cnCacheNodeStatus :: Lens' CacheNode (Maybe Text)
cnCacheNodeStatus =
    lens _cnCacheNodeStatus (\s a -> s { _cnCacheNodeStatus = a })

-- | The date and time when the cache node was created.
cnCacheNodeCreateTime :: Lens' CacheNode (Maybe ISO8601)
cnCacheNodeCreateTime =
    lens _cnCacheNodeCreateTime (\s a -> s { _cnCacheNodeCreateTime = a })

-- | The hostname and IP address for connecting to this cache node.
cnEndpoint :: Lens' CacheNode (Maybe Endpoint)
cnEndpoint = lens _cnEndpoint (\s a -> s { _cnEndpoint = a })

-- | The status of the parameter group applied to this cache node.
cnParameterGroupStatus :: Lens' CacheNode (Maybe Text)
cnParameterGroupStatus =
    lens _cnParameterGroupStatus (\s a -> s { _cnParameterGroupStatus = a })

-- | The ID of the primary node to which this read replica node is synchronized.
-- If this field is empty, then this node is not associated with a primary
-- cache cluster.
cnSourceCacheNodeId :: Lens' CacheNode (Maybe Text)
cnSourceCacheNodeId =
    lens _cnSourceCacheNodeId (\s a -> s { _cnSourceCacheNodeId = a })

-- | The Availability Zone where this node was created and now resides.
cnCustomerAvailabilityZone :: Lens' CacheNode (Maybe Text)
cnCustomerAvailabilityZone =
    lens _cnCustomerAvailabilityZone
         (\s a -> s { _cnCustomerAvailabilityZone = a })

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
    , _cntspDescription :: Maybe Text
    , _cntspSource :: Maybe Text
    , _cntspDataType :: Maybe Text
    , _cntspAllowedValues :: Maybe Text
    , _cntspIsModifiable :: Maybe Bool
    , _cntspMinimumEngineVersion :: Maybe Text
    , _cntspCacheNodeTypeSpecificValues :: [CacheNodeTypeSpecificValue]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheNodeTypeSpecificParameter' data type.
--
-- 'CacheNodeTypeSpecificParameter' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Source ::@ @Maybe Text@
--
-- * @DataType ::@ @Maybe Text@
--
-- * @AllowedValues ::@ @Maybe Text@
--
-- * @IsModifiable ::@ @Maybe Bool@
--
-- * @MinimumEngineVersion ::@ @Maybe Text@
--
-- * @CacheNodeTypeSpecificValues ::@ @[CacheNodeTypeSpecificValue]@
--
mkCacheNodeTypeSpecificParameter :: CacheNodeTypeSpecificParameter
mkCacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter
    { _cntspParameterName = Nothing
    , _cntspDescription = Nothing
    , _cntspSource = Nothing
    , _cntspDataType = Nothing
    , _cntspAllowedValues = Nothing
    , _cntspIsModifiable = Nothing
    , _cntspMinimumEngineVersion = Nothing
    , _cntspCacheNodeTypeSpecificValues = mempty
    }

-- | The name of the parameter.
cntspParameterName :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspParameterName =
    lens _cntspParameterName (\s a -> s { _cntspParameterName = a })

-- | A description of the parameter.
cntspDescription :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDescription =
    lens _cntspDescription (\s a -> s { _cntspDescription = a })

-- | The source of the parameter value.
cntspSource :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspSource = lens _cntspSource (\s a -> s { _cntspSource = a })

-- | The valid data type for the parameter.
cntspDataType :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDataType = lens _cntspDataType (\s a -> s { _cntspDataType = a })

-- | The valid range of values for the parameter.
cntspAllowedValues :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspAllowedValues =
    lens _cntspAllowedValues (\s a -> s { _cntspAllowedValues = a })

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
cntspIsModifiable :: Lens' CacheNodeTypeSpecificParameter (Maybe Bool)
cntspIsModifiable =
    lens _cntspIsModifiable (\s a -> s { _cntspIsModifiable = a })

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspMinimumEngineVersion =
    lens _cntspMinimumEngineVersion
         (\s a -> s { _cntspMinimumEngineVersion = a })

-- | A list of cache node types and their corresponding values for this
-- parameter.
cntspCacheNodeTypeSpecificValues :: Lens' CacheNodeTypeSpecificParameter [CacheNodeTypeSpecificValue]
cntspCacheNodeTypeSpecificValues =
    lens _cntspCacheNodeTypeSpecificValues
         (\s a -> s { _cntspCacheNodeTypeSpecificValues = a })

instance FromXML CacheNodeTypeSpecificParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNodeTypeSpecificParameter"

-- | A value that applies only to a certain cache node type.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { _cntsvCacheNodeType :: Maybe Text
    , _cntsvValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheNodeTypeSpecificValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @Value ::@ @Maybe Text@
--
mkCacheNodeTypeSpecificValue :: CacheNodeTypeSpecificValue
mkCacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { _cntsvCacheNodeType = Nothing
    , _cntsvValue = Nothing
    }

-- | The cache node type for which this value applies.
cntsvCacheNodeType :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvCacheNodeType =
    lens _cntsvCacheNodeType (\s a -> s { _cntsvCacheNodeType = a })

-- | The value for the cache node type.
cntsvValue :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvValue = lens _cntsvValue (\s a -> s { _cntsvValue = a })

instance FromXML CacheNodeTypeSpecificValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNodeTypeSpecificValue"

instance ToQuery CacheNodeTypeSpecificValue where
    toQuery = genericQuery def

-- | Represents the output of a CreateCacheParameterGroup operation.
data CacheParameterGroup = CacheParameterGroup
    { _cpgCacheParameterGroupName :: Maybe Text
    , _cpgCacheParameterGroupFamily :: Maybe Text
    , _cpgDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheParameterGroup' data type.
--
-- 'CacheParameterGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupName ::@ @Maybe Text@
--
-- * @CacheParameterGroupFamily ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
mkCacheParameterGroup :: CacheParameterGroup
mkCacheParameterGroup = CacheParameterGroup
    { _cpgCacheParameterGroupName = Nothing
    , _cpgCacheParameterGroupFamily = Nothing
    , _cpgDescription = Nothing
    }

-- | The name of the cache parameter group.
cpgCacheParameterGroupName :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupName =
    lens _cpgCacheParameterGroupName
         (\s a -> s { _cpgCacheParameterGroupName = a })

-- | The name of the cache parameter group family that this cache parameter
-- group is compatible with.
cpgCacheParameterGroupFamily :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupFamily =
    lens _cpgCacheParameterGroupFamily
         (\s a -> s { _cpgCacheParameterGroupFamily = a })

-- | The description for this cache parameter group.
cpgDescription :: Lens' CacheParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\s a -> s { _cpgDescription = a })

instance FromXML CacheParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroup"

-- | The status of the cache parameter group.
data CacheParameterGroupStatus = CacheParameterGroupStatus
    { _cpgsCacheParameterGroupName :: Maybe Text
    , _cpgsParameterApplyStatus :: Maybe Text
    , _cpgsCacheNodeIdsToReboot :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheParameterGroupStatus' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupName ::@ @Maybe Text@
--
-- * @ParameterApplyStatus ::@ @Maybe Text@
--
-- * @CacheNodeIdsToReboot ::@ @[Text]@
--
mkCacheParameterGroupStatus :: CacheParameterGroupStatus
mkCacheParameterGroupStatus = CacheParameterGroupStatus
    { _cpgsCacheParameterGroupName = Nothing
    , _cpgsParameterApplyStatus = Nothing
    , _cpgsCacheNodeIdsToReboot = mempty
    }

-- | The name of the cache parameter group.
cpgsCacheParameterGroupName :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsCacheParameterGroupName =
    lens _cpgsCacheParameterGroupName
         (\s a -> s { _cpgsCacheParameterGroupName = a })

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus =
    lens _cpgsParameterApplyStatus
         (\s a -> s { _cpgsParameterApplyStatus = a })

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cpgsCacheNodeIdsToReboot :: Lens' CacheParameterGroupStatus [Text]
cpgsCacheNodeIdsToReboot =
    lens _cpgsCacheNodeIdsToReboot
         (\s a -> s { _cpgsCacheNodeIdsToReboot = a })

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
    , _csgCacheSecurityGroupName :: Maybe Text
    , _csgDescription :: Maybe Text
    , _csgEC2SecurityGroups :: [EC2SecurityGroup]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheSecurityGroup' data type.
--
-- 'CacheSecurityGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OwnerId ::@ @Maybe Text@
--
-- * @CacheSecurityGroupName ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @EC2SecurityGroups ::@ @[EC2SecurityGroup]@
--
mkCacheSecurityGroup :: CacheSecurityGroup
mkCacheSecurityGroup = CacheSecurityGroup
    { _csgOwnerId = Nothing
    , _csgCacheSecurityGroupName = Nothing
    , _csgDescription = Nothing
    , _csgEC2SecurityGroups = mempty
    }

-- | The AWS account ID of the cache security group owner.
csgOwnerId :: Lens' CacheSecurityGroup (Maybe Text)
csgOwnerId = lens _csgOwnerId (\s a -> s { _csgOwnerId = a })

-- | The name of the cache security group.
csgCacheSecurityGroupName :: Lens' CacheSecurityGroup (Maybe Text)
csgCacheSecurityGroupName =
    lens _csgCacheSecurityGroupName
         (\s a -> s { _csgCacheSecurityGroupName = a })

-- | The description of the cache security group.
csgDescription :: Lens' CacheSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
csgEC2SecurityGroups :: Lens' CacheSecurityGroup [EC2SecurityGroup]
csgEC2SecurityGroups =
    lens _csgEC2SecurityGroups (\s a -> s { _csgEC2SecurityGroups = a })

instance FromXML CacheSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroup"

-- | Represents a cache cluster's status within a particular cache security
-- group.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership
    { _csgmCacheSecurityGroupName :: Maybe Text
    , _csgmStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheSecurityGroupMembership' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSecurityGroupName ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
mkCacheSecurityGroupMembership :: CacheSecurityGroupMembership
mkCacheSecurityGroupMembership = CacheSecurityGroupMembership
    { _csgmCacheSecurityGroupName = Nothing
    , _csgmStatus = Nothing
    }

-- | The name of the cache security group.
csgmCacheSecurityGroupName :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmCacheSecurityGroupName =
    lens _csgmCacheSecurityGroupName
         (\s a -> s { _csgmCacheSecurityGroupName = a })

-- | The membership status in the cache security group. The status changes when
-- a cache security group is modified, or when the cache security groups
-- assigned to a cache cluster are modified.
csgmStatus :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s { _csgmStatus = a })

instance FromXML CacheSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroup"

instance ToQuery CacheSecurityGroupMembership where
    toQuery = genericQuery def

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
data CacheSubnetGroup = CacheSubnetGroup
    { _csgrCacheSubnetGroupName :: Maybe Text
    , _csgrCacheSubnetGroupDescription :: Maybe Text
    , _csgrVpcId :: Maybe Text
    , _csgrSubnets :: [Subnet]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'CacheSubnetGroup' data type.
--
-- 'CacheSubnetGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheSubnetGroupName ::@ @Maybe Text@
--
-- * @CacheSubnetGroupDescription ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @Subnets ::@ @[Subnet]@
--
mkCacheSubnetGroup :: CacheSubnetGroup
mkCacheSubnetGroup = CacheSubnetGroup
    { _csgrCacheSubnetGroupName = Nothing
    , _csgrCacheSubnetGroupDescription = Nothing
    , _csgrVpcId = Nothing
    , _csgrSubnets = mempty
    }

-- | The name of the cache subnet group.
csgrCacheSubnetGroupName :: Lens' CacheSubnetGroup (Maybe Text)
csgrCacheSubnetGroupName =
    lens _csgrCacheSubnetGroupName
         (\s a -> s { _csgrCacheSubnetGroupName = a })

-- | The description of the cache subnet group.
csgrCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
csgrCacheSubnetGroupDescription =
    lens _csgrCacheSubnetGroupDescription
         (\s a -> s { _csgrCacheSubnetGroupDescription = a })

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
csgrVpcId :: Lens' CacheSubnetGroup (Maybe Text)
csgrVpcId = lens _csgrVpcId (\s a -> s { _csgrVpcId = a })

-- | A list of subnets associated with the cache subnet group.
csgrSubnets :: Lens' CacheSubnetGroup [Subnet]
csgrSubnets = lens _csgrSubnets (\s a -> s { _csgrSubnets = a })

instance FromXML CacheSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSubnetGroup"

-- | Provides ownership and status information for an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
    , _ecsgEC2SecurityGroupName :: Maybe Text
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EC2SecurityGroup' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Status ::@ @Maybe Text@
--
-- * @EC2SecurityGroupName ::@ @Maybe Text@
--
-- * @EC2SecurityGroupOwnerId ::@ @Maybe Text@
--
mkEC2SecurityGroup :: EC2SecurityGroup
mkEC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus = Nothing
    , _ecsgEC2SecurityGroupName = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }

-- | The status of the Amazon EC2 security group.
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })

-- | The name of the Amazon EC2 security group.
ecsgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupName =
    lens _ecsgEC2SecurityGroupName
         (\s a -> s { _ecsgEC2SecurityGroupName = a })

-- | The AWS account ID of the Amazon EC2 security group owner.
ecsgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
ecsgEC2SecurityGroupOwnerId =
    lens _ecsgEC2SecurityGroupOwnerId
         (\s a -> s { _ecsgEC2SecurityGroupOwnerId = a })

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericQuery def

-- | Represents the information required for client programs to connect to a
-- cache node.
data Endpoint = Endpoint
    { _eAddress :: Maybe Text
    , _ePort :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Endpoint' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Address ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
mkEndpoint :: Endpoint
mkEndpoint = Endpoint
    { _eAddress = Nothing
    , _ePort = Nothing
    }

-- | The DNS hostname of the cache node.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\s a -> s { _eAddress = a })

-- | The port number that the cache engine is listening on.
ePort :: Lens' Endpoint (Maybe Integer)
ePort = lens _ePort (\s a -> s { _ePort = a })

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericQuery def

-- | Represents the output of a DescribeEngineDefaultParameters operation.
data EngineDefaults = EngineDefaults
    { _edCacheParameterGroupFamily :: Maybe Text
    , _edMarker :: Maybe Text
    , _edParameters :: [Parameter]
    , _edCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'EngineDefaults' data type.
--
-- 'EngineDefaults' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupFamily ::@ @Maybe Text@
--
-- * @Marker ::@ @Maybe Text@
--
-- * @Parameters ::@ @[Parameter]@
--
-- * @CacheNodeTypeSpecificParameters ::@ @[CacheNodeTypeSpecificParameter]@
--
mkEngineDefaults :: EngineDefaults
mkEngineDefaults = EngineDefaults
    { _edCacheParameterGroupFamily = Nothing
    , _edMarker = Nothing
    , _edParameters = mempty
    , _edCacheNodeTypeSpecificParameters = mempty
    }

-- | Specifies the name of the cache parameter group family to which the engine
-- default parameters apply.
edCacheParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edCacheParameterGroupFamily =
    lens _edCacheParameterGroupFamily
         (\s a -> s { _edCacheParameterGroupFamily = a })

-- | Provides an identifier to allow retrieval of paginated results.
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\s a -> s { _edMarker = a })

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\s a -> s { _edParameters = a })

-- | A list of parameters specific to a particular cache node type. Each element
-- in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters :: Lens' EngineDefaults [CacheNodeTypeSpecificParameter]
edCacheNodeTypeSpecificParameters =
    lens _edCacheNodeTypeSpecificParameters
         (\s a -> s { _edCacheNodeTypeSpecificParameters = a })

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

-- | Represents a single occurrence of something interesting within the system.
-- Some examples of events are creating a cache cluster, adding or removing a
-- cache node, or rebooting a node.
data Event = Event
    { _erSourceIdentifier :: Maybe Text
    , _erSourceType :: Maybe SourceType
    , _erMessage :: Maybe Text
    , _erDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Event' data type.
--
-- 'Event' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceIdentifier ::@ @Maybe Text@
--
-- * @SourceType ::@ @Maybe SourceType@
--
-- * @Message ::@ @Maybe Text@
--
-- * @Date ::@ @Maybe ISO8601@
--
mkEvent :: Event
mkEvent = Event
    { _erSourceIdentifier = Nothing
    , _erSourceType = Nothing
    , _erMessage = Nothing
    , _erDate = Nothing
    }

-- | The identifier for the source of the event. For example, if the event
-- occurred at the cache cluster level, the identifier would be the name of
-- the cache cluster.
erSourceIdentifier :: Lens' Event (Maybe Text)
erSourceIdentifier =
    lens _erSourceIdentifier (\s a -> s { _erSourceIdentifier = a })

-- | Specifies the origin of this event - a cache cluster, a parameter group, a
-- security group, etc.
erSourceType :: Lens' Event (Maybe SourceType)
erSourceType = lens _erSourceType (\s a -> s { _erSourceType = a })

-- | The text of the event.
erMessage :: Lens' Event (Maybe Text)
erMessage = lens _erMessage (\s a -> s { _erMessage = a })

-- | The date and time when the event occurred.
erDate :: Lens' Event (Maybe ISO8601)
erDate = lens _erDate (\s a -> s { _erDate = a })

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | Represents a collection of cache nodes in a replication group.
data NodeGroup = NodeGroup
    { _ngNodeGroupId :: Maybe Text
    , _ngStatus :: Maybe Text
    , _ngPrimaryEndpoint :: Maybe Endpoint
    , _ngNodeGroupMembers :: [NodeGroupMember]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NodeGroup' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NodeGroupId ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @PrimaryEndpoint ::@ @Maybe Endpoint@
--
-- * @NodeGroupMembers ::@ @[NodeGroupMember]@
--
mkNodeGroup :: NodeGroup
mkNodeGroup = NodeGroup
    { _ngNodeGroupId = Nothing
    , _ngStatus = Nothing
    , _ngPrimaryEndpoint = Nothing
    , _ngNodeGroupMembers = mempty
    }

-- | The identifier for the node group. A replication group contains only one
-- node group; therefore, the node group ID is 0001.
ngNodeGroupId :: Lens' NodeGroup (Maybe Text)
ngNodeGroupId = lens _ngNodeGroupId (\s a -> s { _ngNodeGroupId = a })

-- | The current state of this replication group - creating, available, etc.
ngStatus :: Lens' NodeGroup (Maybe Text)
ngStatus = lens _ngStatus (\s a -> s { _ngStatus = a })

-- | Represents the information required for client programs to connect to a
-- cache node.
ngPrimaryEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngPrimaryEndpoint =
    lens _ngPrimaryEndpoint (\s a -> s { _ngPrimaryEndpoint = a })

-- | A list containing information about individual nodes within the node group.
ngNodeGroupMembers :: Lens' NodeGroup [NodeGroupMember]
ngNodeGroupMembers =
    lens _ngNodeGroupMembers (\s a -> s { _ngNodeGroupMembers = a })

instance FromXML NodeGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroup"

instance ToQuery NodeGroup where
    toQuery = genericQuery def

-- | Represents a single node within a node group.
data NodeGroupMember = NodeGroupMember
    { _ngmCacheClusterId :: Maybe Text
    , _ngmCacheNodeId :: Maybe Text
    , _ngmReadEndpoint :: Maybe Endpoint
    , _ngmPreferredAvailabilityZone :: Maybe Text
    , _ngmCurrentRole :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NodeGroupMember' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheClusterId ::@ @Maybe Text@
--
-- * @CacheNodeId ::@ @Maybe Text@
--
-- * @ReadEndpoint ::@ @Maybe Endpoint@
--
-- * @PreferredAvailabilityZone ::@ @Maybe Text@
--
-- * @CurrentRole ::@ @Maybe Text@
--
mkNodeGroupMember :: NodeGroupMember
mkNodeGroupMember = NodeGroupMember
    { _ngmCacheClusterId = Nothing
    , _ngmCacheNodeId = Nothing
    , _ngmReadEndpoint = Nothing
    , _ngmPreferredAvailabilityZone = Nothing
    , _ngmCurrentRole = Nothing
    }

-- | The ID of the cache cluster to which the node belongs.
ngmCacheClusterId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheClusterId =
    lens _ngmCacheClusterId (\s a -> s { _ngmCacheClusterId = a })

-- | The ID of the node within its cache cluster. A node ID is a numeric
-- identifier (0001, 0002, etc.).
ngmCacheNodeId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheNodeId = lens _ngmCacheNodeId (\s a -> s { _ngmCacheNodeId = a })

-- | Represents the information required for client programs to connect to a
-- cache node.
ngmReadEndpoint :: Lens' NodeGroupMember (Maybe Endpoint)
ngmReadEndpoint = lens _ngmReadEndpoint (\s a -> s { _ngmReadEndpoint = a })

-- | The name of the Availability Zone in which the node is located.
ngmPreferredAvailabilityZone :: Lens' NodeGroupMember (Maybe Text)
ngmPreferredAvailabilityZone =
    lens _ngmPreferredAvailabilityZone
         (\s a -> s { _ngmPreferredAvailabilityZone = a })

-- | The role that is currently assigned to the node - primary or replica.
ngmCurrentRole :: Lens' NodeGroupMember (Maybe Text)
ngmCurrentRole = lens _ngmCurrentRole (\s a -> s { _ngmCurrentRole = a })

instance FromXML NodeGroupMember where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroupMember"

instance ToQuery NodeGroupMember where
    toQuery = genericQuery def

-- | Represents an individual cache node in a snapshot of a cache cluster.
data NodeSnapshot = NodeSnapshot
    { _nsCacheNodeId :: Maybe Text
    , _nsCacheSize :: Maybe Text
    , _nsCacheNodeCreateTime :: Maybe ISO8601
    , _nsSnapshotCreateTime :: Maybe ISO8601
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NodeSnapshot' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheNodeId ::@ @Maybe Text@
--
-- * @CacheSize ::@ @Maybe Text@
--
-- * @CacheNodeCreateTime ::@ @Maybe ISO8601@
--
-- * @SnapshotCreateTime ::@ @Maybe ISO8601@
--
mkNodeSnapshot :: NodeSnapshot
mkNodeSnapshot = NodeSnapshot
    { _nsCacheNodeId = Nothing
    , _nsCacheSize = Nothing
    , _nsCacheNodeCreateTime = Nothing
    , _nsSnapshotCreateTime = Nothing
    }

-- | The cache node identifier for the node in the source cache cluster.
nsCacheNodeId :: Lens' NodeSnapshot (Maybe Text)
nsCacheNodeId = lens _nsCacheNodeId (\s a -> s { _nsCacheNodeId = a })

-- | The size of the cache on the source cache node.
nsCacheSize :: Lens' NodeSnapshot (Maybe Text)
nsCacheSize = lens _nsCacheSize (\s a -> s { _nsCacheSize = a })

-- | The date and time when the cache node was created in the source cache
-- cluster.
nsCacheNodeCreateTime :: Lens' NodeSnapshot (Maybe ISO8601)
nsCacheNodeCreateTime =
    lens _nsCacheNodeCreateTime (\s a -> s { _nsCacheNodeCreateTime = a })

-- | The date and time when the source node's metadata and cache data set was
-- obtained for the snapshot.
nsSnapshotCreateTime :: Lens' NodeSnapshot (Maybe ISO8601)
nsSnapshotCreateTime =
    lens _nsSnapshotCreateTime (\s a -> s { _nsSnapshotCreateTime = a })

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
    , _ncTopicStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'NotificationConfiguration' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicArn ::@ @Maybe Text@
--
-- * @TopicStatus ::@ @Maybe Text@
--
mkNotificationConfiguration :: NotificationConfiguration
mkNotificationConfiguration = NotificationConfiguration
    { _ncTopicArn = Nothing
    , _ncTopicStatus = Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the topic.
ncTopicArn :: Lens' NotificationConfiguration (Maybe Text)
ncTopicArn = lens _ncTopicArn (\s a -> s { _ncTopicArn = a })

-- | The current state of the topic.
ncTopicStatus :: Lens' NotificationConfiguration (Maybe Text)
ncTopicStatus = lens _ncTopicStatus (\s a -> s { _ncTopicStatus = a })

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NotificationConfiguration"

instance ToQuery NotificationConfiguration where
    toQuery = genericQuery def

-- | Describes an individual setting that controls some aspect of ElastiCache
-- behavior.
data Parameter = Parameter
    { _pParameterName :: Maybe Text
    , _pParameterValue :: Maybe Text
    , _pDescription :: Maybe Text
    , _pSource :: Maybe Text
    , _pDataType :: Maybe Text
    , _pAllowedValues :: Maybe Text
    , _pIsModifiable :: Maybe Bool
    , _pMinimumEngineVersion :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Parameter' data type.
--
-- 'Parameter' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterName ::@ @Maybe Text@
--
-- * @ParameterValue ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Source ::@ @Maybe Text@
--
-- * @DataType ::@ @Maybe Text@
--
-- * @AllowedValues ::@ @Maybe Text@
--
-- * @IsModifiable ::@ @Maybe Bool@
--
-- * @MinimumEngineVersion ::@ @Maybe Text@
--
mkParameter :: Parameter
mkParameter = Parameter
    { _pParameterName = Nothing
    , _pParameterValue = Nothing
    , _pDescription = Nothing
    , _pSource = Nothing
    , _pDataType = Nothing
    , _pAllowedValues = Nothing
    , _pIsModifiable = Nothing
    , _pMinimumEngineVersion = Nothing
    }

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s { _pParameterName = a })

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s { _pParameterValue = a })

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s { _pDescription = a })

-- | The source of the parameter.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s { _pSource = a })

-- | The valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s { _pDataType = a })

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s { _pAllowedValues = a })

-- | Indicates whether (true) or not (false) the parameter can be modified. Some
-- parameters have security or operational implications that prevent them from
-- being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s { _pIsModifiable = a })

-- | The earliest cache engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion =
    lens _pMinimumEngineVersion (\s a -> s { _pMinimumEngineVersion = a })

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

-- | Describes a name-value pair that is used to update the value of a
-- parameter.
data ParameterNameValue = ParameterNameValue
    { _pnvParameterName :: Maybe Text
    , _pnvParameterValue :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ParameterNameValue' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ParameterName ::@ @Maybe Text@
--
-- * @ParameterValue ::@ @Maybe Text@
--
mkParameterNameValue :: ParameterNameValue
mkParameterNameValue = ParameterNameValue
    { _pnvParameterName = Nothing
    , _pnvParameterValue = Nothing
    }

-- | The name of the parameter.
pnvParameterName :: Lens' ParameterNameValue (Maybe Text)
pnvParameterName =
    lens _pnvParameterName (\s a -> s { _pnvParameterName = a })

-- | The value of the parameter.
pnvParameterValue :: Lens' ParameterNameValue (Maybe Text)
pnvParameterValue =
    lens _pnvParameterValue (\s a -> s { _pnvParameterValue = a })

instance ToQuery ParameterNameValue where
    toQuery = genericQuery def

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
data PendingModifiedValues = PendingModifiedValues
    { _pmvNumCacheNodes :: Maybe Integer
    , _pmvCacheNodeIdsToRemove :: [Text]
    , _pmvEngineVersion :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'PendingModifiedValues' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @NumCacheNodes ::@ @Maybe Integer@
--
-- * @CacheNodeIdsToRemove ::@ @[Text]@
--
-- * @EngineVersion ::@ @Maybe Text@
--
mkPendingModifiedValues :: PendingModifiedValues
mkPendingModifiedValues = PendingModifiedValues
    { _pmvNumCacheNodes = Nothing
    , _pmvCacheNodeIdsToRemove = mempty
    , _pmvEngineVersion = Nothing
    }

-- | The new number of cache nodes for the cache cluster.
pmvNumCacheNodes :: Lens' PendingModifiedValues (Maybe Integer)
pmvNumCacheNodes =
    lens _pmvNumCacheNodes (\s a -> s { _pmvNumCacheNodes = a })

-- | A list of cache node IDs that are being removed (or will be removed) from
-- the cache cluster. A node ID is a numeric identifier (0001, 0002, etc.).
pmvCacheNodeIdsToRemove :: Lens' PendingModifiedValues [Text]
pmvCacheNodeIdsToRemove =
    lens _pmvCacheNodeIdsToRemove
         (\s a -> s { _pmvCacheNodeIdsToRemove = a })

-- | The new cache engine version that the cache cluster will run.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion =
    lens _pmvEngineVersion (\s a -> s { _pmvEngineVersion = a })

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericQuery def

-- | Contains the specific price and frequency of a recurring charges for a
-- reserved cache node, or for a reserved cache node offering.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount :: Maybe Double
    , _rcRecurringChargeFrequency :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'RecurringCharge' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @RecurringChargeAmount ::@ @Maybe Double@
--
-- * @RecurringChargeFrequency ::@ @Maybe Text@
--
mkRecurringCharge :: RecurringCharge
mkRecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }

-- | The monetary amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount =
    lens _rcRecurringChargeAmount
         (\s a -> s { _rcRecurringChargeAmount = a })

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency =
    lens _rcRecurringChargeFrequency
         (\s a -> s { _rcRecurringChargeFrequency = a })

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericQuery def

-- | Contains all of the attributes of a specific replication group.
data ReplicationGroup = ReplicationGroup
    { _rgReplicationGroupId :: Maybe Text
    , _rgDescription :: Maybe Text
    , _rgStatus :: Maybe Text
    , _rgPendingModifiedValues :: Maybe ReplicationGroupPendingModifiedValues
    , _rgMemberClusters :: [Text]
    , _rgNodeGroups :: [NodeGroup]
    , _rgSnapshottingClusterId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReplicationGroup' data type.
--
-- 'ReplicationGroup' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReplicationGroupId ::@ @Maybe Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
-- * @PendingModifiedValues ::@ @Maybe ReplicationGroupPendingModifiedValues@
--
-- * @MemberClusters ::@ @[Text]@
--
-- * @NodeGroups ::@ @[NodeGroup]@
--
-- * @SnapshottingClusterId ::@ @Maybe Text@
--
mkReplicationGroup :: ReplicationGroup
mkReplicationGroup = ReplicationGroup
    { _rgReplicationGroupId = Nothing
    , _rgDescription = Nothing
    , _rgStatus = Nothing
    , _rgPendingModifiedValues = Nothing
    , _rgMemberClusters = mempty
    , _rgNodeGroups = mempty
    , _rgSnapshottingClusterId = Nothing
    }

-- | The identifier for the replication group.
rgReplicationGroupId :: Lens' ReplicationGroup (Maybe Text)
rgReplicationGroupId =
    lens _rgReplicationGroupId (\s a -> s { _rgReplicationGroupId = a })

-- | The description of the replication group.
rgDescription :: Lens' ReplicationGroup (Maybe Text)
rgDescription = lens _rgDescription (\s a -> s { _rgDescription = a })

-- | The current state of this replication group - creating, available, etc.
rgStatus :: Lens' ReplicationGroup (Maybe Text)
rgStatus = lens _rgStatus (\s a -> s { _rgStatus = a })

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
rgPendingModifiedValues :: Lens' ReplicationGroup (Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues =
    lens _rgPendingModifiedValues
         (\s a -> s { _rgPendingModifiedValues = a })

-- | The names of all the cache clusters that are part of this replication
-- group.
rgMemberClusters :: Lens' ReplicationGroup [Text]
rgMemberClusters =
    lens _rgMemberClusters (\s a -> s { _rgMemberClusters = a })

-- | A single element list with information about the nodes in the replication
-- group.
rgNodeGroups :: Lens' ReplicationGroup [NodeGroup]
rgNodeGroups = lens _rgNodeGroups (\s a -> s { _rgNodeGroups = a })

-- | The cache cluster ID that is used as the daily snapshot source for the
-- replication group.
rgSnapshottingClusterId :: Lens' ReplicationGroup (Maybe Text)
rgSnapshottingClusterId =
    lens _rgSnapshottingClusterId
         (\s a -> s { _rgSnapshottingClusterId = a })

instance FromXML ReplicationGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroup"

-- | Represents the output of a PurchaseReservedCacheNodesOffering operation.
data ReservedCacheNode = ReservedCacheNode
    { _rcnReservedCacheNodeId :: Maybe Text
    , _rcnReservedCacheNodesOfferingId :: Maybe Text
    , _rcnCacheNodeType :: Maybe Text
    , _rcnStartTime :: Maybe ISO8601
    , _rcnDuration :: Maybe Integer
    , _rcnFixedPrice :: Maybe Double
    , _rcnUsagePrice :: Maybe Double
    , _rcnCacheNodeCount :: Maybe Integer
    , _rcnProductDescription :: Maybe Text
    , _rcnOfferingType :: Maybe Text
    , _rcnState :: Maybe Text
    , _rcnRecurringCharges :: [RecurringCharge]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedCacheNode' data type.
--
-- 'ReservedCacheNode' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedCacheNodeId ::@ @Maybe Text@
--
-- * @ReservedCacheNodesOfferingId ::@ @Maybe Text@
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @CacheNodeCount ::@ @Maybe Integer@
--
-- * @ProductDescription ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @State ::@ @Maybe Text@
--
-- * @RecurringCharges ::@ @[RecurringCharge]@
--
mkReservedCacheNode :: ReservedCacheNode
mkReservedCacheNode = ReservedCacheNode
    { _rcnReservedCacheNodeId = Nothing
    , _rcnReservedCacheNodesOfferingId = Nothing
    , _rcnCacheNodeType = Nothing
    , _rcnStartTime = Nothing
    , _rcnDuration = Nothing
    , _rcnFixedPrice = Nothing
    , _rcnUsagePrice = Nothing
    , _rcnCacheNodeCount = Nothing
    , _rcnProductDescription = Nothing
    , _rcnOfferingType = Nothing
    , _rcnState = Nothing
    , _rcnRecurringCharges = mempty
    }

-- | The unique identifier for the reservation.
rcnReservedCacheNodeId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodeId =
    lens _rcnReservedCacheNodeId (\s a -> s { _rcnReservedCacheNodeId = a })

-- | The offering identifier.
rcnReservedCacheNodesOfferingId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodesOfferingId =
    lens _rcnReservedCacheNodesOfferingId
         (\s a -> s { _rcnReservedCacheNodesOfferingId = a })

-- | The cache node type for the reserved cache nodes.
rcnCacheNodeType :: Lens' ReservedCacheNode (Maybe Text)
rcnCacheNodeType =
    lens _rcnCacheNodeType (\s a -> s { _rcnCacheNodeType = a })

-- | The time the reservation started.
rcnStartTime :: Lens' ReservedCacheNode (Maybe ISO8601)
rcnStartTime = lens _rcnStartTime (\s a -> s { _rcnStartTime = a })

-- | The duration of the reservation in seconds.
rcnDuration :: Lens' ReservedCacheNode (Maybe Integer)
rcnDuration = lens _rcnDuration (\s a -> s { _rcnDuration = a })

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice :: Lens' ReservedCacheNode (Maybe Double)
rcnFixedPrice = lens _rcnFixedPrice (\s a -> s { _rcnFixedPrice = a })

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice :: Lens' ReservedCacheNode (Maybe Double)
rcnUsagePrice = lens _rcnUsagePrice (\s a -> s { _rcnUsagePrice = a })

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount :: Lens' ReservedCacheNode (Maybe Integer)
rcnCacheNodeCount =
    lens _rcnCacheNodeCount (\s a -> s { _rcnCacheNodeCount = a })

-- | The description of the reserved cache node.
rcnProductDescription :: Lens' ReservedCacheNode (Maybe Text)
rcnProductDescription =
    lens _rcnProductDescription (\s a -> s { _rcnProductDescription = a })

-- | The offering type of this reserved cache node.
rcnOfferingType :: Lens' ReservedCacheNode (Maybe Text)
rcnOfferingType = lens _rcnOfferingType (\s a -> s { _rcnOfferingType = a })

-- | The state of the reserved cache node.
rcnState :: Lens' ReservedCacheNode (Maybe Text)
rcnState = lens _rcnState (\s a -> s { _rcnState = a })

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges :: Lens' ReservedCacheNode [RecurringCharge]
rcnRecurringCharges =
    lens _rcnRecurringCharges (\s a -> s { _rcnRecurringCharges = a })

instance FromXML ReservedCacheNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNode"

-- | Describes all of the attributes of a reserved cache node offering.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering
    { _rcnoReservedCacheNodesOfferingId :: Maybe Text
    , _rcnoCacheNodeType :: Maybe Text
    , _rcnoDuration :: Maybe Integer
    , _rcnoFixedPrice :: Maybe Double
    , _rcnoUsagePrice :: Maybe Double
    , _rcnoProductDescription :: Maybe Text
    , _rcnoOfferingType :: Maybe Text
    , _rcnoRecurringCharges :: [RecurringCharge]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ReservedCacheNodesOffering' data type.
--
-- 'ReservedCacheNodesOffering' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedCacheNodesOfferingId ::@ @Maybe Text@
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @Duration ::@ @Maybe Integer@
--
-- * @FixedPrice ::@ @Maybe Double@
--
-- * @UsagePrice ::@ @Maybe Double@
--
-- * @ProductDescription ::@ @Maybe Text@
--
-- * @OfferingType ::@ @Maybe Text@
--
-- * @RecurringCharges ::@ @[RecurringCharge]@
--
mkReservedCacheNodesOffering :: ReservedCacheNodesOffering
mkReservedCacheNodesOffering = ReservedCacheNodesOffering
    { _rcnoReservedCacheNodesOfferingId = Nothing
    , _rcnoCacheNodeType = Nothing
    , _rcnoDuration = Nothing
    , _rcnoFixedPrice = Nothing
    , _rcnoUsagePrice = Nothing
    , _rcnoProductDescription = Nothing
    , _rcnoOfferingType = Nothing
    , _rcnoRecurringCharges = mempty
    }

-- | A unique identifier for the reserved cache node offering.
rcnoReservedCacheNodesOfferingId :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoReservedCacheNodesOfferingId =
    lens _rcnoReservedCacheNodesOfferingId
         (\s a -> s { _rcnoReservedCacheNodesOfferingId = a })

-- | The cache node type for the reserved cache node.
rcnoCacheNodeType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoCacheNodeType =
    lens _rcnoCacheNodeType (\s a -> s { _rcnoCacheNodeType = a })

-- | The duration of the offering. in seconds.
rcnoDuration :: Lens' ReservedCacheNodesOffering (Maybe Integer)
rcnoDuration = lens _rcnoDuration (\s a -> s { _rcnoDuration = a })

-- | The fixed price charged for this offering.
rcnoFixedPrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoFixedPrice = lens _rcnoFixedPrice (\s a -> s { _rcnoFixedPrice = a })

-- | The hourly price charged for this offering.
rcnoUsagePrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoUsagePrice = lens _rcnoUsagePrice (\s a -> s { _rcnoUsagePrice = a })

-- | The cache engine used by the offering.
rcnoProductDescription :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoProductDescription =
    lens _rcnoProductDescription (\s a -> s { _rcnoProductDescription = a })

-- | The offering type.
rcnoOfferingType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoOfferingType =
    lens _rcnoOfferingType (\s a -> s { _rcnoOfferingType = a })

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges :: Lens' ReservedCacheNodesOffering [RecurringCharge]
rcnoRecurringCharges =
    lens _rcnoRecurringCharges (\s a -> s { _rcnoRecurringCharges = a })

instance FromXML ReservedCacheNodesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNodesOffering"

-- | Represents a single cache security group and its status..
data SecurityGroupMembership = SecurityGroupMembership
    { _sgmSecurityGroupId :: Maybe Text
    , _sgmStatus :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'SecurityGroupMembership' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SecurityGroupId ::@ @Maybe Text@
--
-- * @Status ::@ @Maybe Text@
--
mkSecurityGroupMembership :: SecurityGroupMembership
mkSecurityGroupMembership = SecurityGroupMembership
    { _sgmSecurityGroupId = Nothing
    , _sgmStatus = Nothing
    }

-- | The identifier of the cache security group.
sgmSecurityGroupId :: Lens' SecurityGroupMembership (Maybe Text)
sgmSecurityGroupId =
    lens _sgmSecurityGroupId (\s a -> s { _sgmSecurityGroupId = a })

-- | The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cache cluster are modified.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus = lens _sgmStatus (\s a -> s { _sgmStatus = a })

instance FromXML SecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SecurityGroupMembership"

instance ToQuery SecurityGroupMembership where
    toQuery = genericQuery def

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
data Snapshot = Snapshot
    { _sSnapshotName :: Maybe Text
    , _sCacheClusterId :: Maybe Text
    , _sSnapshotStatus :: Maybe Text
    , _sSnapshotSource :: Maybe Text
    , _sCacheNodeType :: Maybe Text
    , _sEngine :: Maybe Text
    , _sEngineVersion :: Maybe Text
    , _sNumCacheNodes :: Maybe Integer
    , _sPreferredAvailabilityZone :: Maybe Text
    , _sCacheClusterCreateTime :: Maybe ISO8601
    , _sPreferredMaintenanceWindow :: Maybe Text
    , _sTopicArn :: Maybe Text
    , _sPort :: Maybe Integer
    , _sCacheParameterGroupName :: Maybe Text
    , _sCacheSubnetGroupName :: Maybe Text
    , _sVpcId :: Maybe Text
    , _sAutoMinorVersionUpgrade :: Maybe Bool
    , _sSnapshotRetentionLimit :: Maybe Integer
    , _sSnapshotWindow :: Maybe Text
    , _sNodeSnapshots :: [NodeSnapshot]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Snapshot' data type.
--
-- 'Snapshot' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SnapshotName ::@ @Maybe Text@
--
-- * @CacheClusterId ::@ @Maybe Text@
--
-- * @SnapshotStatus ::@ @Maybe Text@
--
-- * @SnapshotSource ::@ @Maybe Text@
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @Engine ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @NumCacheNodes ::@ @Maybe Integer@
--
-- * @PreferredAvailabilityZone ::@ @Maybe Text@
--
-- * @CacheClusterCreateTime ::@ @Maybe ISO8601@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @TopicArn ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @CacheParameterGroupName ::@ @Maybe Text@
--
-- * @CacheSubnetGroupName ::@ @Maybe Text@
--
-- * @VpcId ::@ @Maybe Text@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @SnapshotRetentionLimit ::@ @Maybe Integer@
--
-- * @SnapshotWindow ::@ @Maybe Text@
--
-- * @NodeSnapshots ::@ @[NodeSnapshot]@
--
mkSnapshot :: Snapshot
mkSnapshot = Snapshot
    { _sSnapshotName = Nothing
    , _sCacheClusterId = Nothing
    , _sSnapshotStatus = Nothing
    , _sSnapshotSource = Nothing
    , _sCacheNodeType = Nothing
    , _sEngine = Nothing
    , _sEngineVersion = Nothing
    , _sNumCacheNodes = Nothing
    , _sPreferredAvailabilityZone = Nothing
    , _sCacheClusterCreateTime = Nothing
    , _sPreferredMaintenanceWindow = Nothing
    , _sTopicArn = Nothing
    , _sPort = Nothing
    , _sCacheParameterGroupName = Nothing
    , _sCacheSubnetGroupName = Nothing
    , _sVpcId = Nothing
    , _sAutoMinorVersionUpgrade = Nothing
    , _sSnapshotRetentionLimit = Nothing
    , _sSnapshotWindow = Nothing
    , _sNodeSnapshots = mempty
    }

-- | The name of a snapshot. For an automatic snapshot, the name is
-- system-generated; for a manual snapshot, this is the user-provided name.
sSnapshotName :: Lens' Snapshot (Maybe Text)
sSnapshotName = lens _sSnapshotName (\s a -> s { _sSnapshotName = a })

-- | The user-supplied identifier of the source cache cluster.
sCacheClusterId :: Lens' Snapshot (Maybe Text)
sCacheClusterId = lens _sCacheClusterId (\s a -> s { _sCacheClusterId = a })

-- | The status of the snapshot. Valid values: creating | available | restoring
-- | copying | deleting.
sSnapshotStatus :: Lens' Snapshot (Maybe Text)
sSnapshotStatus = lens _sSnapshotStatus (\s a -> s { _sSnapshotStatus = a })

-- | Indicates whether the snapshot is from an automatic backup (automated) or
-- was created manually (manual).
sSnapshotSource :: Lens' Snapshot (Maybe Text)
sSnapshotSource = lens _sSnapshotSource (\s a -> s { _sSnapshotSource = a })

-- | The name of the compute and memory capacity node type for the source cache
-- cluster.
sCacheNodeType :: Lens' Snapshot (Maybe Text)
sCacheNodeType = lens _sCacheNodeType (\s a -> s { _sCacheNodeType = a })

-- | The name of the cache engine (memcached or redis) used by the source cache
-- cluster.
sEngine :: Lens' Snapshot (Maybe Text)
sEngine = lens _sEngine (\s a -> s { _sEngine = a })

-- | The version of the cache engine version that is used by the source cache
-- cluster.
sEngineVersion :: Lens' Snapshot (Maybe Text)
sEngineVersion = lens _sEngineVersion (\s a -> s { _sEngineVersion = a })

-- | The number of cache nodes in the source cache cluster.
sNumCacheNodes :: Lens' Snapshot (Maybe Integer)
sNumCacheNodes = lens _sNumCacheNodes (\s a -> s { _sNumCacheNodes = a })

-- | The name of the Availability Zone in which the source cache cluster is
-- located.
sPreferredAvailabilityZone :: Lens' Snapshot (Maybe Text)
sPreferredAvailabilityZone =
    lens _sPreferredAvailabilityZone
         (\s a -> s { _sPreferredAvailabilityZone = a })

-- | The date and time when the source cache cluster was created.
sCacheClusterCreateTime :: Lens' Snapshot (Maybe ISO8601)
sCacheClusterCreateTime =
    lens _sCacheClusterCreateTime
         (\s a -> s { _sCacheClusterCreateTime = a })

-- | The time range (in UTC) during which weekly system maintenance can occur on
-- the source cache cluster.
sPreferredMaintenanceWindow :: Lens' Snapshot (Maybe Text)
sPreferredMaintenanceWindow =
    lens _sPreferredMaintenanceWindow
         (\s a -> s { _sPreferredMaintenanceWindow = a })

-- | The Amazon Resource Name (ARN) for the topic used by the source cache
-- cluster for publishing notifications.
sTopicArn :: Lens' Snapshot (Maybe Text)
sTopicArn = lens _sTopicArn (\s a -> s { _sTopicArn = a })

-- | The port number used by each cache nodes in the source cache cluster.
sPort :: Lens' Snapshot (Maybe Integer)
sPort = lens _sPort (\s a -> s { _sPort = a })

-- | The cache parameter group that is associated with the source cache cluster.
sCacheParameterGroupName :: Lens' Snapshot (Maybe Text)
sCacheParameterGroupName =
    lens _sCacheParameterGroupName
         (\s a -> s { _sCacheParameterGroupName = a })

-- | The name of the cache subnet group associated with the source cache
-- cluster.
sCacheSubnetGroupName :: Lens' Snapshot (Maybe Text)
sCacheSubnetGroupName =
    lens _sCacheSubnetGroupName (\s a -> s { _sCacheSubnetGroupName = a })

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cache cluster.
sVpcId :: Lens' Snapshot (Maybe Text)
sVpcId = lens _sVpcId (\s a -> s { _sVpcId = a })

-- | For the source cache cluster, indicates whether minor version patches are
-- applied automatically (true) or not (false).
sAutoMinorVersionUpgrade :: Lens' Snapshot (Maybe Bool)
sAutoMinorVersionUpgrade =
    lens _sAutoMinorVersionUpgrade
         (\s a -> s { _sAutoMinorVersionUpgrade = a })

-- | For an automatic snapshot, the number of days for which ElastiCache will
-- retain the snapshot before deleting it. For manual snapshots, this field
-- reflects the SnapshotRetentionLimit for the source cache cluster when the
-- snapshot was created. This field is otherwise ignored: Manual snapshots do
-- not expire, and can only be deleted using the DeleteSnapshot action.
-- ImportantIf the value of SnapshotRetentionLimit is set to zero (0), backups
-- are turned off.
sSnapshotRetentionLimit :: Lens' Snapshot (Maybe Integer)
sSnapshotRetentionLimit =
    lens _sSnapshotRetentionLimit
         (\s a -> s { _sSnapshotRetentionLimit = a })

-- | The daily time range during which ElastiCache takes daily snapshots of the
-- source cache cluster.
sSnapshotWindow :: Lens' Snapshot (Maybe Text)
sSnapshotWindow = lens _sSnapshotWindow (\s a -> s { _sSnapshotWindow = a })

-- | A list of the cache nodes in the source cache cluster.
sNodeSnapshots :: Lens' Snapshot [NodeSnapshot]
sNodeSnapshots = lens _sNodeSnapshots (\s a -> s { _sNodeSnapshots = a })

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Snapshot"

-- | Represents the subnet associated with a cache cluster. This parameter
-- refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and
-- used with ElastiCache.
data Subnet = Subnet
    { _srSubnetIdentifier :: Maybe Text
    , _srSubnetAvailabilityZone :: Maybe AvailabilityZone
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Subnet' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubnetIdentifier ::@ @Maybe Text@
--
-- * @SubnetAvailabilityZone ::@ @Maybe AvailabilityZone@
--
mkSubnet :: Subnet
mkSubnet = Subnet
    { _srSubnetIdentifier = Nothing
    , _srSubnetAvailabilityZone = Nothing
    }

-- | The unique identifier for the subnet.
srSubnetIdentifier :: Lens' Subnet (Maybe Text)
srSubnetIdentifier =
    lens _srSubnetIdentifier (\s a -> s { _srSubnetIdentifier = a })

-- | The Availability Zone associated with the subnet.
srSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
srSubnetAvailabilityZone =
    lens _srSubnetAvailabilityZone
         (\s a -> s { _srSubnetAvailabilityZone = a })

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericQuery def
