{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ElastiCache.Types
    (
    -- * Service
      ElastiCache
    -- ** Error
    , RESTError
    -- ** XML
    , xmlOptions

    -- * NodeSnapshot
    , NodeSnapshot
    , nodeSnapshot
    , nsCacheNodeCreateTime
    , nsCacheNodeId
    , nsCacheSize
    , nsSnapshotCreateTime

    -- * Snapshot
    , Snapshot
    , snapshot
    , sAutoMinorVersionUpgrade
    , sCacheClusterCreateTime
    , sCacheClusterId
    , sCacheNodeType
    , sCacheParameterGroupName
    , sCacheSubnetGroupName
    , sEngine
    , sEngineVersion
    , sNodeSnapshots
    , sNumCacheNodes
    , sPort
    , sPreferredAvailabilityZone
    , sPreferredMaintenanceWindow
    , sSnapshotName
    , sSnapshotRetentionLimit
    , sSnapshotSource
    , sSnapshotStatus
    , sSnapshotWindow
    , sTopicArn
    , sVpcId

    -- * Event
    , Event
    , event
    , eDate
    , eMessage
    , eSourceIdentifier
    , eSourceType

    -- * NodeGroup
    , NodeGroup
    , nodeGroup
    , ngNodeGroupId
    , ngNodeGroupMembers
    , ngPrimaryEndpoint
    , ngStatus

    -- * CacheNodeTypeSpecificValue
    , CacheNodeTypeSpecificValue
    , cacheNodeTypeSpecificValue
    , cntsvCacheNodeType
    , cntsvValue

    -- * PendingAutomaticFailoverStatus
    , PendingAutomaticFailoverStatus (..)

    -- * NotificationConfiguration
    , NotificationConfiguration
    , notificationConfiguration
    , ncTopicArn
    , ncTopicStatus

    -- * ReplicationGroupPendingModifiedValues
    , ReplicationGroupPendingModifiedValues
    , replicationGroupPendingModifiedValues
    , rgpmvAutomaticFailoverStatus
    , rgpmvPrimaryClusterId

    -- * EC2SecurityGroup
    , EC2SecurityGroup
    , ec2SecurityGroup
    , ecsgEC2SecurityGroupName
    , ecsgEC2SecurityGroupOwnerId
    , ecsgStatus

    -- * ParameterNameValue
    , ParameterNameValue
    , parameterNameValue
    , pnvParameterName
    , pnvParameterValue

    -- * SourceType
    , SourceType (..)

    -- * CacheSubnetGroup
    , CacheSubnetGroup
    , cacheSubnetGroup
    , csgCacheSubnetGroupDescription
    , csgCacheSubnetGroupName
    , csgSubnets
    , csgVpcId

    -- * ReservedCacheNode
    , ReservedCacheNode
    , reservedCacheNode
    , rcnCacheNodeCount
    , rcnCacheNodeType
    , rcnDuration
    , rcnFixedPrice
    , rcnOfferingType
    , rcnProductDescription
    , rcnRecurringCharges
    , rcnReservedCacheNodeId
    , rcnReservedCacheNodesOfferingId
    , rcnStartTime
    , rcnState
    , rcnUsagePrice

    -- * Subnet
    , Subnet
    , subnet
    , sSubnetAvailabilityZone
    , sSubnetIdentifier

    -- * SecurityGroupMembership
    , SecurityGroupMembership
    , securityGroupMembership
    , sgmSecurityGroupId
    , sgmStatus

    -- * CacheCluster
    , CacheCluster
    , cacheCluster
    , ccAutoMinorVersionUpgrade
    , ccCacheClusterCreateTime
    , ccCacheClusterId
    , ccCacheClusterStatus
    , ccCacheNodeType
    , ccCacheNodes
    , ccCacheParameterGroup
    , ccCacheSecurityGroups
    , ccCacheSubnetGroupName
    , ccClientDownloadLandingPage
    , ccConfigurationEndpoint
    , ccEngine
    , ccEngineVersion
    , ccNotificationConfiguration
    , ccNumCacheNodes
    , ccPendingModifiedValues
    , ccPreferredAvailabilityZone
    , ccPreferredMaintenanceWindow
    , ccReplicationGroupId
    , ccSecurityGroups
    , ccSnapshotRetentionLimit
    , ccSnapshotWindow

    -- * EngineDefaults
    , EngineDefaults
    , engineDefaults
    , edCacheNodeTypeSpecificParameters
    , edCacheParameterGroupFamily
    , edMarker
    , edParameters

    -- * CacheParameterGroupStatus
    , CacheParameterGroupStatus
    , cacheParameterGroupStatus
    , cpgsCacheNodeIdsToReboot
    , cpgsCacheParameterGroupName
    , cpgsParameterApplyStatus

    -- * CacheNode
    , CacheNode
    , cacheNode
    , cnCacheNodeCreateTime
    , cnCacheNodeId
    , cnCacheNodeStatus
    , cnCustomerAvailabilityZone
    , cnEndpoint
    , cnParameterGroupStatus
    , cnSourceCacheNodeId

    -- * CacheSecurityGroupMembership
    , CacheSecurityGroupMembership
    , cacheSecurityGroupMembership
    , csgmCacheSecurityGroupName
    , csgmStatus

    -- * AvailabilityZone
    , AvailabilityZone
    , availabilityZone
    , azName

    -- * NodeGroupMember
    , NodeGroupMember
    , nodeGroupMember
    , ngmCacheClusterId
    , ngmCacheNodeId
    , ngmCurrentRole
    , ngmPreferredAvailabilityZone
    , ngmReadEndpoint

    -- * CacheParameterGroup
    , CacheParameterGroup
    , cacheParameterGroup
    , cpgCacheParameterGroupFamily
    , cpgCacheParameterGroupName
    , cpgDescription

    -- * AutomaticFailoverStatus
    , AutomaticFailoverStatus (..)

    -- * CacheSecurityGroup
    , CacheSecurityGroup
    , cacheSecurityGroup
    , csgCacheSecurityGroupName
    , csgDescription
    , csgEC2SecurityGroups
    , csgOwnerId

    -- * CacheNodeTypeSpecificParameter
    , CacheNodeTypeSpecificParameter
    , cacheNodeTypeSpecificParameter
    , cntspAllowedValues
    , cntspCacheNodeTypeSpecificValues
    , cntspDataType
    , cntspDescription
    , cntspIsModifiable
    , cntspMinimumEngineVersion
    , cntspParameterName
    , cntspSource

    -- * AZMode
    , AZMode (..)

    -- * CacheEngineVersion
    , CacheEngineVersion
    , cacheEngineVersion
    , cevCacheEngineDescription
    , cevCacheEngineVersionDescription
    , cevCacheParameterGroupFamily
    , cevEngine
    , cevEngineVersion

    -- * ReplicationGroup
    , ReplicationGroup
    , replicationGroup
    , rgAutomaticFailover
    , rgDescription
    , rgMemberClusters
    , rgNodeGroups
    , rgPendingModifiedValues
    , rgReplicationGroupId
    , rgSnapshottingClusterId
    , rgStatus

    -- * RecurringCharge
    , RecurringCharge
    , recurringCharge
    , rcRecurringChargeAmount
    , rcRecurringChargeFrequency

    -- * ReservedCacheNodesOffering
    , ReservedCacheNodesOffering
    , reservedCacheNodesOffering
    , rcnoCacheNodeType
    , rcnoDuration
    , rcnoFixedPrice
    , rcnoOfferingType
    , rcnoProductDescription
    , rcnoRecurringCharges
    , rcnoReservedCacheNodesOfferingId
    , rcnoUsagePrice

    -- * Endpoint
    , Endpoint
    , endpoint
    , eAddress
    , ePort

    -- * PendingModifiedValues
    , PendingModifiedValues
    , pendingModifiedValues
    , pmvCacheNodeIdsToRemove
    , pmvEngineVersion
    , pmvNumCacheNodes

    -- * CacheParameterGroupNameMessage
    , CacheParameterGroupNameMessage
    , cacheParameterGroupNameMessage
    , cpgnmCacheParameterGroupName

    -- * Parameter
    , Parameter
    , parameter
    , pAllowedValues
    , pDataType
    , pDescription
    , pIsModifiable
    , pMinimumEngineVersion
    , pParameterName
    , pParameterValue
    , pSource
    ) where

import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Supported version (@2014-09-30@) of the Amazon ElastiCache.
data ElastiCache deriving (Typeable)

instance AWSService ElastiCache where
    type Sg ElastiCache = V4
    type Er ElastiCache = RESTError

    service = Service
        { _svcEndpoint = regional
        , _svcAbbrev   = "ElastiCache"
        , _svcPrefix   = "elasticache"
        , _svcVersion  = "2014-09-30"
        , _svcTarget   = Nothing
        }

    handle = restError alwaysFail

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://elasticache.amazonaws.com/doc/2014-09-30/"
    }

data NodeSnapshot = NodeSnapshot
    { _nsCacheNodeCreateTime :: Maybe RFC822
    , _nsCacheNodeId         :: Maybe Text
    , _nsCacheSize           :: Maybe Text
    , _nsSnapshotCreateTime  :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'NodeSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nsCacheNodeCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'nsCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'nsCacheSize' @::@ 'Maybe' 'Text'
--
-- * 'nsSnapshotCreateTime' @::@ 'Maybe' 'UTCTime'
--
nodeSnapshot :: NodeSnapshot
nodeSnapshot = NodeSnapshot
    { _nsCacheNodeId         = Nothing
    , _nsCacheSize           = Nothing
    , _nsCacheNodeCreateTime = Nothing
    , _nsSnapshotCreateTime  = Nothing
    }

-- | The date and time when the cache node was created in the source cache
-- cluster.
nsCacheNodeCreateTime :: Lens' NodeSnapshot (Maybe UTCTime)
nsCacheNodeCreateTime =
    lens _nsCacheNodeCreateTime (\s a -> s { _nsCacheNodeCreateTime = a })
        . mapping _Time

-- | The cache node identifier for the node in the source cache cluster.
nsCacheNodeId :: Lens' NodeSnapshot (Maybe Text)
nsCacheNodeId = lens _nsCacheNodeId (\s a -> s { _nsCacheNodeId = a })

-- | The size of the cache on the source cache node.
nsCacheSize :: Lens' NodeSnapshot (Maybe Text)
nsCacheSize = lens _nsCacheSize (\s a -> s { _nsCacheSize = a })

-- | The date and time when the source node's metadata and cache data set was
-- obtained for the snapshot.
nsSnapshotCreateTime :: Lens' NodeSnapshot (Maybe UTCTime)
nsSnapshotCreateTime =
    lens _nsSnapshotCreateTime (\s a -> s { _nsSnapshotCreateTime = a })
        . mapping _Time

instance FromXML NodeSnapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeSnapshot"

instance ToQuery NodeSnapshot

data Snapshot = Snapshot
    { _sAutoMinorVersionUpgrade    :: Maybe Bool
    , _sCacheClusterCreateTime     :: Maybe RFC822
    , _sCacheClusterId             :: Maybe Text
    , _sCacheNodeType              :: Maybe Text
    , _sCacheParameterGroupName    :: Maybe Text
    , _sCacheSubnetGroupName       :: Maybe Text
    , _sEngine                     :: Maybe Text
    , _sEngineVersion              :: Maybe Text
    , _sNodeSnapshots              :: [NodeSnapshot]
    , _sNumCacheNodes              :: Maybe Int
    , _sPort                       :: Maybe Int
    , _sPreferredAvailabilityZone  :: Maybe Text
    , _sPreferredMaintenanceWindow :: Maybe Text
    , _sSnapshotName               :: Maybe Text
    , _sSnapshotRetentionLimit     :: Maybe Int
    , _sSnapshotSource             :: Maybe Text
    , _sSnapshotStatus             :: Maybe Text
    , _sSnapshotWindow             :: Maybe Text
    , _sTopicArn                   :: Maybe Text
    , _sVpcId                      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Snapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'sCacheClusterCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'sCacheClusterId' @::@ 'Maybe' 'Text'
--
-- * 'sCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'sCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'sCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'sEngine' @::@ 'Maybe' 'Text'
--
-- * 'sEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'sNodeSnapshots' @::@ ['NodeSnapshot']
--
-- * 'sNumCacheNodes' @::@ 'Maybe' 'Int'
--
-- * 'sPort' @::@ 'Maybe' 'Int'
--
-- * 'sPreferredAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'sPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'sSnapshotName' @::@ 'Maybe' 'Text'
--
-- * 'sSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'sSnapshotSource' @::@ 'Maybe' 'Text'
--
-- * 'sSnapshotStatus' @::@ 'Maybe' 'Text'
--
-- * 'sSnapshotWindow' @::@ 'Maybe' 'Text'
--
-- * 'sTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'sVpcId' @::@ 'Maybe' 'Text'
--
snapshot :: Snapshot
snapshot = Snapshot
    { _sSnapshotName               = Nothing
    , _sCacheClusterId             = Nothing
    , _sSnapshotStatus             = Nothing
    , _sSnapshotSource             = Nothing
    , _sCacheNodeType              = Nothing
    , _sEngine                     = Nothing
    , _sEngineVersion              = Nothing
    , _sNumCacheNodes              = Nothing
    , _sPreferredAvailabilityZone  = Nothing
    , _sCacheClusterCreateTime     = Nothing
    , _sPreferredMaintenanceWindow = Nothing
    , _sTopicArn                   = Nothing
    , _sPort                       = Nothing
    , _sCacheParameterGroupName    = Nothing
    , _sCacheSubnetGroupName       = Nothing
    , _sVpcId                      = Nothing
    , _sAutoMinorVersionUpgrade    = Nothing
    , _sSnapshotRetentionLimit     = Nothing
    , _sSnapshotWindow             = Nothing
    , _sNodeSnapshots              = mempty
    }

-- | For the source cache cluster, indicates whether minor version patches are
-- applied automatically (true) or not (false).
sAutoMinorVersionUpgrade :: Lens' Snapshot (Maybe Bool)
sAutoMinorVersionUpgrade =
    lens _sAutoMinorVersionUpgrade
        (\s a -> s { _sAutoMinorVersionUpgrade = a })

-- | The date and time when the source cache cluster was created.
sCacheClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
sCacheClusterCreateTime =
    lens _sCacheClusterCreateTime (\s a -> s { _sCacheClusterCreateTime = a })
        . mapping _Time

-- | The user-supplied identifier of the source cache cluster.
sCacheClusterId :: Lens' Snapshot (Maybe Text)
sCacheClusterId = lens _sCacheClusterId (\s a -> s { _sCacheClusterId = a })

-- | The name of the compute and memory capacity node type for the source
-- cache cluster.
sCacheNodeType :: Lens' Snapshot (Maybe Text)
sCacheNodeType = lens _sCacheNodeType (\s a -> s { _sCacheNodeType = a })

-- | The cache parameter group that is associated with the source cache
-- cluster.
sCacheParameterGroupName :: Lens' Snapshot (Maybe Text)
sCacheParameterGroupName =
    lens _sCacheParameterGroupName
        (\s a -> s { _sCacheParameterGroupName = a })

-- | The name of the cache subnet group associated with the source cache
-- cluster.
sCacheSubnetGroupName :: Lens' Snapshot (Maybe Text)
sCacheSubnetGroupName =
    lens _sCacheSubnetGroupName (\s a -> s { _sCacheSubnetGroupName = a })

-- | The name of the cache engine (memcached or redis) used by the source
-- cache cluster.
sEngine :: Lens' Snapshot (Maybe Text)
sEngine = lens _sEngine (\s a -> s { _sEngine = a })

-- | The version of the cache engine version that is used by the source cache
-- cluster.
sEngineVersion :: Lens' Snapshot (Maybe Text)
sEngineVersion = lens _sEngineVersion (\s a -> s { _sEngineVersion = a })

-- | A list of the cache nodes in the source cache cluster.
sNodeSnapshots :: Lens' Snapshot [NodeSnapshot]
sNodeSnapshots = lens _sNodeSnapshots (\s a -> s { _sNodeSnapshots = a })

-- | The number of cache nodes in the source cache cluster.
sNumCacheNodes :: Lens' Snapshot (Maybe Int)
sNumCacheNodes = lens _sNumCacheNodes (\s a -> s { _sNumCacheNodes = a })

-- | The port number used by each cache nodes in the source cache cluster.
sPort :: Lens' Snapshot (Maybe Int)
sPort = lens _sPort (\s a -> s { _sPort = a })

-- | The name of the Availability Zone in which the source cache cluster is
-- located.
sPreferredAvailabilityZone :: Lens' Snapshot (Maybe Text)
sPreferredAvailabilityZone =
    lens _sPreferredAvailabilityZone
        (\s a -> s { _sPreferredAvailabilityZone = a })

-- | The time range (in UTC) during which weekly system maintenance can occur
-- on the source cache cluster.
sPreferredMaintenanceWindow :: Lens' Snapshot (Maybe Text)
sPreferredMaintenanceWindow =
    lens _sPreferredMaintenanceWindow
        (\s a -> s { _sPreferredMaintenanceWindow = a })

-- | The name of a snapshot. For an automatic snapshot, the name is
-- system-generated; for a manual snapshot, this is the user-provided name.
sSnapshotName :: Lens' Snapshot (Maybe Text)
sSnapshotName = lens _sSnapshotName (\s a -> s { _sSnapshotName = a })

-- | For an automatic snapshot, the number of days for which ElastiCache will
-- retain the snapshot before deleting it. For manual snapshots, this field
-- reflects the SnapshotRetentionLimit for the source cache cluster when the
-- snapshot was created. This field is otherwise ignored: Manual snapshots
-- do not expire, and can only be deleted using the DeleteSnapshot action.
-- ImportantIf the value of SnapshotRetentionLimit is set to zero (0),
-- backups are turned off.
sSnapshotRetentionLimit :: Lens' Snapshot (Maybe Int)
sSnapshotRetentionLimit =
    lens _sSnapshotRetentionLimit (\s a -> s { _sSnapshotRetentionLimit = a })

-- | Indicates whether the snapshot is from an automatic backup (automated) or
-- was created manually (manual).
sSnapshotSource :: Lens' Snapshot (Maybe Text)
sSnapshotSource = lens _sSnapshotSource (\s a -> s { _sSnapshotSource = a })

-- | The status of the snapshot. Valid values: creating | available |
-- restoring | copying | deleting.
sSnapshotStatus :: Lens' Snapshot (Maybe Text)
sSnapshotStatus = lens _sSnapshotStatus (\s a -> s { _sSnapshotStatus = a })

-- | The daily time range during which ElastiCache takes daily snapshots of
-- the source cache cluster.
sSnapshotWindow :: Lens' Snapshot (Maybe Text)
sSnapshotWindow = lens _sSnapshotWindow (\s a -> s { _sSnapshotWindow = a })

-- | The Amazon Resource Name (ARN) for the topic used by the source cache
-- cluster for publishing notifications.
sTopicArn :: Lens' Snapshot (Maybe Text)
sTopicArn = lens _sTopicArn (\s a -> s { _sTopicArn = a })

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cache cluster.
sVpcId :: Lens' Snapshot (Maybe Text)
sVpcId = lens _sVpcId (\s a -> s { _sVpcId = a })

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Snapshot"

instance ToQuery Snapshot

data Event = Event
    { _eDate             :: Maybe RFC822
    , _eMessage          :: Maybe Text
    , _eSourceIdentifier :: Maybe Text
    , _eSourceType       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Event' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'eMessage' @::@ 'Maybe' 'Text'
--
-- * 'eSourceIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'eSourceType' @::@ 'Maybe' 'Text'
--
event :: Event
event = Event
    { _eSourceIdentifier = Nothing
    , _eSourceType       = Nothing
    , _eMessage          = Nothing
    , _eDate             = Nothing
    }

-- | The date and time when the event occurred.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\s a -> s { _eDate = a })
    . mapping _Time

-- | The text of the event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\s a -> s { _eMessage = a })

-- | The identifier for the source of the event. For example, if the event
-- occurred at the cache cluster level, the identifier would be the name of
-- the cache cluster.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier =
    lens _eSourceIdentifier (\s a -> s { _eSourceIdentifier = a })

-- | Specifies the origin of this event - a cache cluster, a parameter group,
-- a security group, etc.
eSourceType :: Lens' Event (Maybe Text)
eSourceType = lens _eSourceType (\s a -> s { _eSourceType = a })

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

instance ToQuery Event

data NodeGroup = NodeGroup
    { _ngNodeGroupId      :: Maybe Text
    , _ngNodeGroupMembers :: [NodeGroupMember]
    , _ngPrimaryEndpoint  :: Maybe Endpoint
    , _ngStatus           :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'NodeGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ngNodeGroupId' @::@ 'Maybe' 'Text'
--
-- * 'ngNodeGroupMembers' @::@ ['NodeGroupMember']
--
-- * 'ngPrimaryEndpoint' @::@ 'Maybe' 'Endpoint'
--
-- * 'ngStatus' @::@ 'Maybe' 'Text'
--
nodeGroup :: NodeGroup
nodeGroup = NodeGroup
    { _ngNodeGroupId      = Nothing
    , _ngStatus           = Nothing
    , _ngPrimaryEndpoint  = Nothing
    , _ngNodeGroupMembers = mempty
    }

-- | The identifier for the node group. A replication group contains only one
-- node group; therefore, the node group ID is 0001.
ngNodeGroupId :: Lens' NodeGroup (Maybe Text)
ngNodeGroupId = lens _ngNodeGroupId (\s a -> s { _ngNodeGroupId = a })

-- | A list containing information about individual nodes within the node
-- group.
ngNodeGroupMembers :: Lens' NodeGroup [NodeGroupMember]
ngNodeGroupMembers =
    lens _ngNodeGroupMembers (\s a -> s { _ngNodeGroupMembers = a })

ngPrimaryEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngPrimaryEndpoint =
    lens _ngPrimaryEndpoint (\s a -> s { _ngPrimaryEndpoint = a })

-- | The current state of this replication group - creating, available, etc.
ngStatus :: Lens' NodeGroup (Maybe Text)
ngStatus = lens _ngStatus (\s a -> s { _ngStatus = a })

instance FromXML NodeGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroup"

instance ToQuery NodeGroup

data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { _cntsvCacheNodeType :: Maybe Text
    , _cntsvValue         :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CacheNodeTypeSpecificValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cntsvCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'cntsvValue' @::@ 'Maybe' 'Text'
--
cacheNodeTypeSpecificValue :: CacheNodeTypeSpecificValue
cacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { _cntsvCacheNodeType = Nothing
    , _cntsvValue         = Nothing
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

instance ToQuery CacheNodeTypeSpecificValue

data PendingAutomaticFailoverStatus
    = Disabled -- ^ disabled
    | Enabled  -- ^ enabled
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable PendingAutomaticFailoverStatus

instance FromText PendingAutomaticFailoverStatus where
    parser = match "disabled" Disabled
         <|> match "enabled"  Enabled

instance ToText PendingAutomaticFailoverStatus where
    toText = \case
        Disabled -> "disabled"
        Enabled  -> "enabled"

instance FromXML PendingAutomaticFailoverStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingAutomaticFailoverStatus"

instance ToQuery PendingAutomaticFailoverStatus

data NotificationConfiguration = NotificationConfiguration
    { _ncTopicArn    :: Maybe Text
    , _ncTopicStatus :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'NotificationConfiguration' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ncTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'ncTopicStatus' @::@ 'Maybe' 'Text'
--
notificationConfiguration :: NotificationConfiguration
notificationConfiguration = NotificationConfiguration
    { _ncTopicArn    = Nothing
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

instance ToQuery NotificationConfiguration

data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { _rgpmvAutomaticFailoverStatus :: Maybe Text
    , _rgpmvPrimaryClusterId        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ReplicationGroupPendingModifiedValues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgpmvAutomaticFailoverStatus' @::@ 'Maybe' 'Text'
--
-- * 'rgpmvPrimaryClusterId' @::@ 'Maybe' 'Text'
--
replicationGroupPendingModifiedValues :: ReplicationGroupPendingModifiedValues
replicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { _rgpmvPrimaryClusterId        = Nothing
    , _rgpmvAutomaticFailoverStatus = Nothing
    }

-- | Indicates the status of automatic failover for this replication group.
rgpmvAutomaticFailoverStatus :: Lens' ReplicationGroupPendingModifiedValues (Maybe Text)
rgpmvAutomaticFailoverStatus =
    lens _rgpmvAutomaticFailoverStatus
        (\s a -> s { _rgpmvAutomaticFailoverStatus = a })

-- | The primary cluster ID which will be applied immediately (if
-- --apply-immediately was specified), or during the next maintenance
-- window.
rgpmvPrimaryClusterId :: Lens' ReplicationGroupPendingModifiedValues (Maybe Text)
rgpmvPrimaryClusterId =
    lens _rgpmvPrimaryClusterId (\s a -> s { _rgpmvPrimaryClusterId = a })

instance FromXML ReplicationGroupPendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroupPendingModifiedValues"

instance ToQuery ReplicationGroupPendingModifiedValues

data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgEC2SecurityGroupName    :: Maybe Text
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
    , _ecsgStatus                  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EC2SecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ecsgEC2SecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ecsgEC2SecurityGroupOwnerId' @::@ 'Maybe' 'Text'
--
-- * 'ecsgStatus' @::@ 'Maybe' 'Text'
--
ec2SecurityGroup :: EC2SecurityGroup
ec2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus                  = Nothing
    , _ecsgEC2SecurityGroupName    = Nothing
    , _ecsgEC2SecurityGroupOwnerId = Nothing
    }

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

-- | The status of the Amazon EC2 security group.
ecsgStatus :: Lens' EC2SecurityGroup (Maybe Text)
ecsgStatus = lens _ecsgStatus (\s a -> s { _ecsgStatus = a })

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup

data ParameterNameValue = ParameterNameValue
    { _pnvParameterName  :: Maybe Text
    , _pnvParameterValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ParameterNameValue' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pnvParameterName' @::@ 'Maybe' 'Text'
--
-- * 'pnvParameterValue' @::@ 'Maybe' 'Text'
--
parameterNameValue :: ParameterNameValue
parameterNameValue = ParameterNameValue
    { _pnvParameterName  = Nothing
    , _pnvParameterValue = Nothing
    }

-- | The name of the parameter.
pnvParameterName :: Lens' ParameterNameValue (Maybe Text)
pnvParameterName = lens _pnvParameterName (\s a -> s { _pnvParameterName = a })

-- | The value of the parameter.
pnvParameterValue :: Lens' ParameterNameValue (Maybe Text)
pnvParameterValue =
    lens _pnvParameterValue (\s a -> s { _pnvParameterValue = a })

instance FromXML ParameterNameValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ParameterNameValue"

instance ToQuery ParameterNameValue

data SourceType
    = STCacheCluster        -- ^ cache-cluster
    | STCacheParameterGroup -- ^ cache-parameter-group
    | STCacheSecurityGroup  -- ^ cache-security-group
    | STCacheSubnetGroup    -- ^ cache-subnet-group
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable SourceType

instance FromText SourceType where
    parser = match "cache-cluster"         STCacheCluster
         <|> match "cache-parameter-group" STCacheParameterGroup
         <|> match "cache-security-group"  STCacheSecurityGroup
         <|> match "cache-subnet-group"    STCacheSubnetGroup

instance ToText SourceType where
    toText = \case
        STCacheCluster        -> "cache-cluster"
        STCacheParameterGroup -> "cache-parameter-group"
        STCacheSecurityGroup  -> "cache-security-group"
        STCacheSubnetGroup    -> "cache-subnet-group"

instance FromXML SourceType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SourceType"

instance ToQuery SourceType

data CacheSubnetGroup = CacheSubnetGroup
    { _csgCacheSubnetGroupDescription :: Maybe Text
    , _csgCacheSubnetGroupName        :: Maybe Text
    , _csgSubnets                     :: [Subnet]
    , _csgVpcId                       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgCacheSubnetGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'csgCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'csgSubnets' @::@ ['Subnet']
--
-- * 'csgVpcId' @::@ 'Maybe' 'Text'
--
cacheSubnetGroup :: CacheSubnetGroup
cacheSubnetGroup = CacheSubnetGroup
    { _csgCacheSubnetGroupName        = Nothing
    , _csgCacheSubnetGroupDescription = Nothing
    , _csgVpcId                       = Nothing
    , _csgSubnets                     = mempty
    }

-- | The description of the cache subnet group.
csgCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupDescription =
    lens _csgCacheSubnetGroupDescription
        (\s a -> s { _csgCacheSubnetGroupDescription = a })

-- | The name of the cache subnet group.
csgCacheSubnetGroupName :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupName =
    lens _csgCacheSubnetGroupName (\s a -> s { _csgCacheSubnetGroupName = a })

-- | A list of subnets associated with the cache subnet group.
csgSubnets :: Lens' CacheSubnetGroup [Subnet]
csgSubnets = lens _csgSubnets (\s a -> s { _csgSubnets = a })

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
csgVpcId :: Lens' CacheSubnetGroup (Maybe Text)
csgVpcId = lens _csgVpcId (\s a -> s { _csgVpcId = a })

instance FromXML CacheSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSubnetGroup"

instance ToQuery CacheSubnetGroup

data ReservedCacheNode = ReservedCacheNode
    { _rcnCacheNodeCount               :: Maybe Int
    , _rcnCacheNodeType                :: Maybe Text
    , _rcnDuration                     :: Maybe Int
    , _rcnFixedPrice                   :: Maybe Double
    , _rcnOfferingType                 :: Maybe Text
    , _rcnProductDescription           :: Maybe Text
    , _rcnRecurringCharges             :: [RecurringCharge]
    , _rcnReservedCacheNodeId          :: Maybe Text
    , _rcnReservedCacheNodesOfferingId :: Maybe Text
    , _rcnStartTime                    :: Maybe RFC822
    , _rcnState                        :: Maybe Text
    , _rcnUsagePrice                   :: Maybe Double
    } deriving (Eq, Show, Generic)

-- | 'ReservedCacheNode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcnCacheNodeCount' @::@ 'Maybe' 'Int'
--
-- * 'rcnCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'rcnDuration' @::@ 'Maybe' 'Int'
--
-- * 'rcnFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rcnOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'rcnProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'rcnRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rcnReservedCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'rcnReservedCacheNodesOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rcnStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'rcnState' @::@ 'Maybe' 'Text'
--
-- * 'rcnUsagePrice' @::@ 'Maybe' 'Double'
--
reservedCacheNode :: ReservedCacheNode
reservedCacheNode = ReservedCacheNode
    { _rcnReservedCacheNodeId          = Nothing
    , _rcnReservedCacheNodesOfferingId = Nothing
    , _rcnCacheNodeType                = Nothing
    , _rcnStartTime                    = Nothing
    , _rcnDuration                     = Nothing
    , _rcnFixedPrice                   = Nothing
    , _rcnUsagePrice                   = Nothing
    , _rcnCacheNodeCount               = Nothing
    , _rcnProductDescription           = Nothing
    , _rcnOfferingType                 = Nothing
    , _rcnState                        = Nothing
    , _rcnRecurringCharges             = mempty
    }

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount :: Lens' ReservedCacheNode (Maybe Int)
rcnCacheNodeCount =
    lens _rcnCacheNodeCount (\s a -> s { _rcnCacheNodeCount = a })

-- | The cache node type for the reserved cache nodes.
rcnCacheNodeType :: Lens' ReservedCacheNode (Maybe Text)
rcnCacheNodeType = lens _rcnCacheNodeType (\s a -> s { _rcnCacheNodeType = a })

-- | The duration of the reservation in seconds.
rcnDuration :: Lens' ReservedCacheNode (Maybe Int)
rcnDuration = lens _rcnDuration (\s a -> s { _rcnDuration = a })

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice :: Lens' ReservedCacheNode (Maybe Double)
rcnFixedPrice = lens _rcnFixedPrice (\s a -> s { _rcnFixedPrice = a })

-- | The offering type of this reserved cache node.
rcnOfferingType :: Lens' ReservedCacheNode (Maybe Text)
rcnOfferingType = lens _rcnOfferingType (\s a -> s { _rcnOfferingType = a })

-- | The description of the reserved cache node.
rcnProductDescription :: Lens' ReservedCacheNode (Maybe Text)
rcnProductDescription =
    lens _rcnProductDescription (\s a -> s { _rcnProductDescription = a })

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges :: Lens' ReservedCacheNode [RecurringCharge]
rcnRecurringCharges =
    lens _rcnRecurringCharges (\s a -> s { _rcnRecurringCharges = a })

-- | The unique identifier for the reservation.
rcnReservedCacheNodeId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodeId =
    lens _rcnReservedCacheNodeId (\s a -> s { _rcnReservedCacheNodeId = a })

-- | The offering identifier.
rcnReservedCacheNodesOfferingId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodesOfferingId =
    lens _rcnReservedCacheNodesOfferingId
        (\s a -> s { _rcnReservedCacheNodesOfferingId = a })

-- | The time the reservation started.
rcnStartTime :: Lens' ReservedCacheNode (Maybe UTCTime)
rcnStartTime = lens _rcnStartTime (\s a -> s { _rcnStartTime = a })
    . mapping _Time

-- | The state of the reserved cache node.
rcnState :: Lens' ReservedCacheNode (Maybe Text)
rcnState = lens _rcnState (\s a -> s { _rcnState = a })

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice :: Lens' ReservedCacheNode (Maybe Double)
rcnUsagePrice = lens _rcnUsagePrice (\s a -> s { _rcnUsagePrice = a })

instance FromXML ReservedCacheNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNode"

instance ToQuery ReservedCacheNode

data Subnet = Subnet
    { _sSubnetAvailabilityZone :: Maybe AvailabilityZone
    , _sSubnetIdentifier       :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'Subnet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sSubnetAvailabilityZone' @::@ 'Maybe' 'AvailabilityZone'
--
-- * 'sSubnetIdentifier' @::@ 'Maybe' 'Text'
--
subnet :: Subnet
subnet = Subnet
    { _sSubnetIdentifier       = Nothing
    , _sSubnetAvailabilityZone = Nothing
    }

-- | The Availability Zone associated with the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone =
    lens _sSubnetAvailabilityZone (\s a -> s { _sSubnetAvailabilityZone = a })

-- | The unique identifier for the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier =
    lens _sSubnetIdentifier (\s a -> s { _sSubnetIdentifier = a })

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet

data SecurityGroupMembership = SecurityGroupMembership
    { _sgmSecurityGroupId :: Maybe Text
    , _sgmStatus          :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SecurityGroupMembership' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sgmSecurityGroupId' @::@ 'Maybe' 'Text'
--
-- * 'sgmStatus' @::@ 'Maybe' 'Text'
--
securityGroupMembership :: SecurityGroupMembership
securityGroupMembership = SecurityGroupMembership
    { _sgmSecurityGroupId = Nothing
    , _sgmStatus          = Nothing
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

instance ToQuery SecurityGroupMembership

data CacheCluster = CacheCluster
    { _ccAutoMinorVersionUpgrade    :: Maybe Bool
    , _ccCacheClusterCreateTime     :: Maybe RFC822
    , _ccCacheClusterId             :: Maybe Text
    , _ccCacheClusterStatus         :: Maybe Text
    , _ccCacheNodeType              :: Maybe Text
    , _ccCacheNodes                 :: [CacheNode]
    , _ccCacheParameterGroup        :: Maybe CacheParameterGroupStatus
    , _ccCacheSecurityGroups        :: [CacheSecurityGroupMembership]
    , _ccCacheSubnetGroupName       :: Maybe Text
    , _ccClientDownloadLandingPage  :: Maybe Text
    , _ccConfigurationEndpoint      :: Maybe Endpoint
    , _ccEngine                     :: Maybe Text
    , _ccEngineVersion              :: Maybe Text
    , _ccNotificationConfiguration  :: Maybe NotificationConfiguration
    , _ccNumCacheNodes              :: Maybe Int
    , _ccPendingModifiedValues      :: Maybe PendingModifiedValues
    , _ccPreferredAvailabilityZone  :: Maybe Text
    , _ccPreferredMaintenanceWindow :: Maybe Text
    , _ccReplicationGroupId         :: Maybe Text
    , _ccSecurityGroups             :: [SecurityGroupMembership]
    , _ccSnapshotRetentionLimit     :: Maybe Int
    , _ccSnapshotWindow             :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'ccCacheClusterCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'ccCacheClusterId' @::@ 'Maybe' 'Text'
--
-- * 'ccCacheClusterStatus' @::@ 'Maybe' 'Text'
--
-- * 'ccCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'ccCacheNodes' @::@ ['CacheNode']
--
-- * 'ccCacheParameterGroup' @::@ 'Maybe' 'CacheParameterGroupStatus'
--
-- * 'ccCacheSecurityGroups' @::@ ['CacheSecurityGroupMembership']
--
-- * 'ccCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'ccClientDownloadLandingPage' @::@ 'Maybe' 'Text'
--
-- * 'ccConfigurationEndpoint' @::@ 'Maybe' 'Endpoint'
--
-- * 'ccEngine' @::@ 'Maybe' 'Text'
--
-- * 'ccEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'ccNotificationConfiguration' @::@ 'Maybe' 'NotificationConfiguration'
--
-- * 'ccNumCacheNodes' @::@ 'Maybe' 'Int'
--
-- * 'ccPendingModifiedValues' @::@ 'Maybe' 'PendingModifiedValues'
--
-- * 'ccPreferredAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ccPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'ccReplicationGroupId' @::@ 'Maybe' 'Text'
--
-- * 'ccSecurityGroups' @::@ ['SecurityGroupMembership']
--
-- * 'ccSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'ccSnapshotWindow' @::@ 'Maybe' 'Text'
--
cacheCluster :: CacheCluster
cacheCluster = CacheCluster
    { _ccCacheClusterId             = Nothing
    , _ccConfigurationEndpoint      = Nothing
    , _ccClientDownloadLandingPage  = Nothing
    , _ccCacheNodeType              = Nothing
    , _ccEngine                     = Nothing
    , _ccEngineVersion              = Nothing
    , _ccCacheClusterStatus         = Nothing
    , _ccNumCacheNodes              = Nothing
    , _ccPreferredAvailabilityZone  = Nothing
    , _ccCacheClusterCreateTime     = Nothing
    , _ccPreferredMaintenanceWindow = Nothing
    , _ccPendingModifiedValues      = Nothing
    , _ccNotificationConfiguration  = Nothing
    , _ccCacheSecurityGroups        = mempty
    , _ccCacheParameterGroup        = Nothing
    , _ccCacheSubnetGroupName       = Nothing
    , _ccCacheNodes                 = mempty
    , _ccAutoMinorVersionUpgrade    = Nothing
    , _ccSecurityGroups             = mempty
    , _ccReplicationGroupId         = Nothing
    , _ccSnapshotRetentionLimit     = Nothing
    , _ccSnapshotWindow             = Nothing
    }

-- | If true, then minor version patches are applied automatically; if false,
-- then automatic minor version patches are disabled.
ccAutoMinorVersionUpgrade :: Lens' CacheCluster (Maybe Bool)
ccAutoMinorVersionUpgrade =
    lens _ccAutoMinorVersionUpgrade
        (\s a -> s { _ccAutoMinorVersionUpgrade = a })

-- | The date and time when the cache cluster was created.
ccCacheClusterCreateTime :: Lens' CacheCluster (Maybe UTCTime)
ccCacheClusterCreateTime =
    lens _ccCacheClusterCreateTime
        (\s a -> s { _ccCacheClusterCreateTime = a })
            . mapping _Time

-- | The user-supplied identifier of the cache cluster. This identifier is a
-- unique key that identifies a cache cluster.
ccCacheClusterId :: Lens' CacheCluster (Maybe Text)
ccCacheClusterId = lens _ccCacheClusterId (\s a -> s { _ccCacheClusterId = a })

-- | The current state of this cache cluster, one of the following values:
-- available, creating, deleted, deleting, incompatible-network, modifying,
-- rebooting cache cluster nodes, restore-failed, or snapshotting.
ccCacheClusterStatus :: Lens' CacheCluster (Maybe Text)
ccCacheClusterStatus =
    lens _ccCacheClusterStatus (\s a -> s { _ccCacheClusterStatus = a })

-- | The name of the compute and memory capacity node type for the cache
-- cluster.
ccCacheNodeType :: Lens' CacheCluster (Maybe Text)
ccCacheNodeType = lens _ccCacheNodeType (\s a -> s { _ccCacheNodeType = a })

-- | A list of cache nodes that are members of the cache cluster.
ccCacheNodes :: Lens' CacheCluster [CacheNode]
ccCacheNodes = lens _ccCacheNodes (\s a -> s { _ccCacheNodes = a })

ccCacheParameterGroup :: Lens' CacheCluster (Maybe CacheParameterGroupStatus)
ccCacheParameterGroup =
    lens _ccCacheParameterGroup (\s a -> s { _ccCacheParameterGroup = a })

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
ccCacheSecurityGroups :: Lens' CacheCluster [CacheSecurityGroupMembership]
ccCacheSecurityGroups =
    lens _ccCacheSecurityGroups (\s a -> s { _ccCacheSecurityGroups = a })

-- | The name of the cache subnet group associated with the cache cluster.
ccCacheSubnetGroupName :: Lens' CacheCluster (Maybe Text)
ccCacheSubnetGroupName =
    lens _ccCacheSubnetGroupName (\s a -> s { _ccCacheSubnetGroupName = a })

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
ccClientDownloadLandingPage :: Lens' CacheCluster (Maybe Text)
ccClientDownloadLandingPage =
    lens _ccClientDownloadLandingPage
        (\s a -> s { _ccClientDownloadLandingPage = a })

ccConfigurationEndpoint :: Lens' CacheCluster (Maybe Endpoint)
ccConfigurationEndpoint =
    lens _ccConfigurationEndpoint (\s a -> s { _ccConfigurationEndpoint = a })

-- | The name of the cache engine (memcached or redis) to be used for this
-- cache cluster.
ccEngine :: Lens' CacheCluster (Maybe Text)
ccEngine = lens _ccEngine (\s a -> s { _ccEngine = a })

-- | The version of the cache engine version that is used in this cache
-- cluster.
ccEngineVersion :: Lens' CacheCluster (Maybe Text)
ccEngineVersion = lens _ccEngineVersion (\s a -> s { _ccEngineVersion = a })

ccNotificationConfiguration :: Lens' CacheCluster (Maybe NotificationConfiguration)
ccNotificationConfiguration =
    lens _ccNotificationConfiguration
        (\s a -> s { _ccNotificationConfiguration = a })

-- | The number of cache nodes in the cache cluster.
ccNumCacheNodes :: Lens' CacheCluster (Maybe Int)
ccNumCacheNodes = lens _ccNumCacheNodes (\s a -> s { _ccNumCacheNodes = a })

ccPendingModifiedValues :: Lens' CacheCluster (Maybe PendingModifiedValues)
ccPendingModifiedValues =
    lens _ccPendingModifiedValues (\s a -> s { _ccPendingModifiedValues = a })

-- | The name of the Availability Zone in which the cache cluster is located
-- or "Multiple" if the cache nodes are located in different Availability
-- Zones.
ccPreferredAvailabilityZone :: Lens' CacheCluster (Maybe Text)
ccPreferredAvailabilityZone =
    lens _ccPreferredAvailabilityZone
        (\s a -> s { _ccPreferredAvailabilityZone = a })

-- | The time range (in UTC) during which weekly system maintenance can occur.
ccPreferredMaintenanceWindow :: Lens' CacheCluster (Maybe Text)
ccPreferredMaintenanceWindow =
    lens _ccPreferredMaintenanceWindow
        (\s a -> s { _ccPreferredMaintenanceWindow = a })

-- | The replication group to which this cache cluster belongs. If this field
-- is empty, the cache cluster is not associated with any replication group.
ccReplicationGroupId :: Lens' CacheCluster (Maybe Text)
ccReplicationGroupId =
    lens _ccReplicationGroupId (\s a -> s { _ccReplicationGroupId = a })

-- | A list of VPC Security Groups associated with the cache cluster.
ccSecurityGroups :: Lens' CacheCluster [SecurityGroupMembership]
ccSecurityGroups = lens _ccSecurityGroups (\s a -> s { _ccSecurityGroups = a })

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. ImportantIf the value of
-- SnapshotRetentionLimit is set to zero (0), backups are turned off.
ccSnapshotRetentionLimit :: Lens' CacheCluster (Maybe Int)
ccSnapshotRetentionLimit =
    lens _ccSnapshotRetentionLimit
        (\s a -> s { _ccSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your cache cluster. Example: 05:00-09:00.
ccSnapshotWindow :: Lens' CacheCluster (Maybe Text)
ccSnapshotWindow = lens _ccSnapshotWindow (\s a -> s { _ccSnapshotWindow = a })

instance FromXML CacheCluster where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheCluster"

instance ToQuery CacheCluster

data EngineDefaults = EngineDefaults
    { _edCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
    , _edCacheParameterGroupFamily       :: Maybe Text
    , _edMarker                          :: Maybe Text
    , _edParameters                      :: [Parameter]
    } deriving (Eq, Show, Generic)

-- | 'EngineDefaults' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'edCacheNodeTypeSpecificParameters' @::@ ['CacheNodeTypeSpecificParameter']
--
-- * 'edCacheParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'edMarker' @::@ 'Maybe' 'Text'
--
-- * 'edParameters' @::@ ['Parameter']
--
engineDefaults :: EngineDefaults
engineDefaults = EngineDefaults
    { _edCacheParameterGroupFamily       = Nothing
    , _edMarker                          = Nothing
    , _edParameters                      = mempty
    , _edCacheNodeTypeSpecificParameters = mempty
    }

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters :: Lens' EngineDefaults [CacheNodeTypeSpecificParameter]
edCacheNodeTypeSpecificParameters =
    lens _edCacheNodeTypeSpecificParameters
        (\s a -> s { _edCacheNodeTypeSpecificParameters = a })

-- | Specifies the name of the cache parameter group family to which the
-- engine default parameters apply.
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

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

instance ToQuery EngineDefaults

data CacheParameterGroupStatus = CacheParameterGroupStatus
    { _cpgsCacheNodeIdsToReboot    :: [Text]
    , _cpgsCacheParameterGroupName :: Maybe Text
    , _cpgsParameterApplyStatus    :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CacheParameterGroupStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgsCacheNodeIdsToReboot' @::@ ['Text']
--
-- * 'cpgsCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cpgsParameterApplyStatus' @::@ 'Maybe' 'Text'
--
cacheParameterGroupStatus :: CacheParameterGroupStatus
cacheParameterGroupStatus = CacheParameterGroupStatus
    { _cpgsCacheParameterGroupName = Nothing
    , _cpgsParameterApplyStatus    = Nothing
    , _cpgsCacheNodeIdsToReboot    = mempty
    }

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cpgsCacheNodeIdsToReboot :: Lens' CacheParameterGroupStatus [Text]
cpgsCacheNodeIdsToReboot =
    lens _cpgsCacheNodeIdsToReboot
        (\s a -> s { _cpgsCacheNodeIdsToReboot = a })

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

instance FromXML CacheParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroupStatus"

instance ToQuery CacheParameterGroupStatus

data CacheNode = CacheNode
    { _cnCacheNodeCreateTime      :: Maybe RFC822
    , _cnCacheNodeId              :: Maybe Text
    , _cnCacheNodeStatus          :: Maybe Text
    , _cnCustomerAvailabilityZone :: Maybe Text
    , _cnEndpoint                 :: Maybe Endpoint
    , _cnParameterGroupStatus     :: Maybe Text
    , _cnSourceCacheNodeId        :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheNode' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnCacheNodeCreateTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'cnCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'cnCacheNodeStatus' @::@ 'Maybe' 'Text'
--
-- * 'cnCustomerAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cnEndpoint' @::@ 'Maybe' 'Endpoint'
--
-- * 'cnParameterGroupStatus' @::@ 'Maybe' 'Text'
--
-- * 'cnSourceCacheNodeId' @::@ 'Maybe' 'Text'
--
cacheNode :: CacheNode
cacheNode = CacheNode
    { _cnCacheNodeId              = Nothing
    , _cnCacheNodeStatus          = Nothing
    , _cnCacheNodeCreateTime      = Nothing
    , _cnEndpoint                 = Nothing
    , _cnParameterGroupStatus     = Nothing
    , _cnSourceCacheNodeId        = Nothing
    , _cnCustomerAvailabilityZone = Nothing
    }

-- | The date and time when the cache node was created.
cnCacheNodeCreateTime :: Lens' CacheNode (Maybe UTCTime)
cnCacheNodeCreateTime =
    lens _cnCacheNodeCreateTime (\s a -> s { _cnCacheNodeCreateTime = a })
        . mapping _Time

-- | The cache node identifier. A node ID is a numeric identifier (0001, 0002,
-- etc.). The combination of cluster ID and node ID uniquely identifies
-- every cache node used in a customer's AWS account.
cnCacheNodeId :: Lens' CacheNode (Maybe Text)
cnCacheNodeId = lens _cnCacheNodeId (\s a -> s { _cnCacheNodeId = a })

-- | The current state of this cache node.
cnCacheNodeStatus :: Lens' CacheNode (Maybe Text)
cnCacheNodeStatus =
    lens _cnCacheNodeStatus (\s a -> s { _cnCacheNodeStatus = a })

-- | The Availability Zone where this node was created and now resides.
cnCustomerAvailabilityZone :: Lens' CacheNode (Maybe Text)
cnCustomerAvailabilityZone =
    lens _cnCustomerAvailabilityZone
        (\s a -> s { _cnCustomerAvailabilityZone = a })

-- | The hostname and IP address for connecting to this cache node.
cnEndpoint :: Lens' CacheNode (Maybe Endpoint)
cnEndpoint = lens _cnEndpoint (\s a -> s { _cnEndpoint = a })

-- | The status of the parameter group applied to this cache node.
cnParameterGroupStatus :: Lens' CacheNode (Maybe Text)
cnParameterGroupStatus =
    lens _cnParameterGroupStatus (\s a -> s { _cnParameterGroupStatus = a })

-- | The ID of the primary node to which this read replica node is
-- synchronized. If this field is empty, then this node is not associated
-- with a primary cache cluster.
cnSourceCacheNodeId :: Lens' CacheNode (Maybe Text)
cnSourceCacheNodeId =
    lens _cnSourceCacheNodeId (\s a -> s { _cnSourceCacheNodeId = a })

instance FromXML CacheNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNode"

instance ToQuery CacheNode

data CacheSecurityGroupMembership = CacheSecurityGroupMembership
    { _csgmCacheSecurityGroupName :: Maybe Text
    , _csgmStatus                 :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CacheSecurityGroupMembership' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgmCacheSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'csgmStatus' @::@ 'Maybe' 'Text'
--
cacheSecurityGroupMembership :: CacheSecurityGroupMembership
cacheSecurityGroupMembership = CacheSecurityGroupMembership
    { _csgmCacheSecurityGroupName = Nothing
    , _csgmStatus                 = Nothing
    }

-- | The name of the cache security group.
csgmCacheSecurityGroupName :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmCacheSecurityGroupName =
    lens _csgmCacheSecurityGroupName
        (\s a -> s { _csgmCacheSecurityGroupName = a })

-- | The membership status in the cache security group. The status changes
-- when a cache security group is modified, or when the cache security
-- groups assigned to a cache cluster are modified.
csgmStatus :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\s a -> s { _csgmStatus = a })

instance FromXML CacheSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroupMembership"

instance ToQuery CacheSecurityGroupMembership

newtype AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'AvailabilityZone' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'azName' @::@ 'Maybe' 'Text'
--
availabilityZone :: AvailabilityZone
availabilityZone = AvailabilityZone
    { _azName = Nothing
    }

-- | The name of the Availability Zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\s a -> s { _azName = a })

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone

data NodeGroupMember = NodeGroupMember
    { _ngmCacheClusterId            :: Maybe Text
    , _ngmCacheNodeId               :: Maybe Text
    , _ngmCurrentRole               :: Maybe Text
    , _ngmPreferredAvailabilityZone :: Maybe Text
    , _ngmReadEndpoint              :: Maybe Endpoint
    } deriving (Eq, Show, Generic)

-- | 'NodeGroupMember' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ngmCacheClusterId' @::@ 'Maybe' 'Text'
--
-- * 'ngmCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'ngmCurrentRole' @::@ 'Maybe' 'Text'
--
-- * 'ngmPreferredAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'ngmReadEndpoint' @::@ 'Maybe' 'Endpoint'
--
nodeGroupMember :: NodeGroupMember
nodeGroupMember = NodeGroupMember
    { _ngmCacheClusterId            = Nothing
    , _ngmCacheNodeId               = Nothing
    , _ngmReadEndpoint              = Nothing
    , _ngmPreferredAvailabilityZone = Nothing
    , _ngmCurrentRole               = Nothing
    }

-- | The ID of the cache cluster to which the node belongs.
ngmCacheClusterId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheClusterId =
    lens _ngmCacheClusterId (\s a -> s { _ngmCacheClusterId = a })

-- | The ID of the node within its cache cluster. A node ID is a numeric
-- identifier (0001, 0002, etc.).
ngmCacheNodeId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheNodeId = lens _ngmCacheNodeId (\s a -> s { _ngmCacheNodeId = a })

-- | The role that is currently assigned to the node - primary or replica.
ngmCurrentRole :: Lens' NodeGroupMember (Maybe Text)
ngmCurrentRole = lens _ngmCurrentRole (\s a -> s { _ngmCurrentRole = a })

-- | The name of the Availability Zone in which the node is located.
ngmPreferredAvailabilityZone :: Lens' NodeGroupMember (Maybe Text)
ngmPreferredAvailabilityZone =
    lens _ngmPreferredAvailabilityZone
        (\s a -> s { _ngmPreferredAvailabilityZone = a })

ngmReadEndpoint :: Lens' NodeGroupMember (Maybe Endpoint)
ngmReadEndpoint = lens _ngmReadEndpoint (\s a -> s { _ngmReadEndpoint = a })

instance FromXML NodeGroupMember where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroupMember"

instance ToQuery NodeGroupMember

data CacheParameterGroup = CacheParameterGroup
    { _cpgCacheParameterGroupFamily :: Maybe Text
    , _cpgCacheParameterGroupName   :: Maybe Text
    , _cpgDescription               :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CacheParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgCacheParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'cpgCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cpgDescription' @::@ 'Maybe' 'Text'
--
cacheParameterGroup :: CacheParameterGroup
cacheParameterGroup = CacheParameterGroup
    { _cpgCacheParameterGroupName   = Nothing
    , _cpgCacheParameterGroupFamily = Nothing
    , _cpgDescription               = Nothing
    }

-- | The name of the cache parameter group family that this cache parameter
-- group is compatible with.
cpgCacheParameterGroupFamily :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupFamily =
    lens _cpgCacheParameterGroupFamily
        (\s a -> s { _cpgCacheParameterGroupFamily = a })

-- | The name of the cache parameter group.
cpgCacheParameterGroupName :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupName =
    lens _cpgCacheParameterGroupName
        (\s a -> s { _cpgCacheParameterGroupName = a })

-- | The description for this cache parameter group.
cpgDescription :: Lens' CacheParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\s a -> s { _cpgDescription = a })

instance FromXML CacheParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroup"

instance ToQuery CacheParameterGroup

data AutomaticFailoverStatus
    = AFSDisabled  -- ^ disabled
    | AFSDisabling -- ^ disabling
    | AFSEnabled   -- ^ enabled
    | AFSEnabling  -- ^ enabling
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AutomaticFailoverStatus

instance FromText AutomaticFailoverStatus where
    parser = match "disabled"  AFSDisabled
         <|> match "disabling" AFSDisabling
         <|> match "enabled"   AFSEnabled
         <|> match "enabling"  AFSEnabling

instance ToText AutomaticFailoverStatus where
    toText = \case
        AFSDisabled  -> "disabled"
        AFSDisabling -> "disabling"
        AFSEnabled   -> "enabled"
        AFSEnabling  -> "enabling"

instance FromXML AutomaticFailoverStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutomaticFailoverStatus"

instance ToQuery AutomaticFailoverStatus

data CacheSecurityGroup = CacheSecurityGroup
    { _csgCacheSecurityGroupName :: Maybe Text
    , _csgDescription            :: Maybe Text
    , _csgEC2SecurityGroups      :: [EC2SecurityGroup]
    , _csgOwnerId                :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csgCacheSecurityGroupName' @::@ 'Maybe' 'Text'
--
-- * 'csgDescription' @::@ 'Maybe' 'Text'
--
-- * 'csgEC2SecurityGroups' @::@ ['EC2SecurityGroup']
--
-- * 'csgOwnerId' @::@ 'Maybe' 'Text'
--
cacheSecurityGroup :: CacheSecurityGroup
cacheSecurityGroup = CacheSecurityGroup
    { _csgOwnerId                = Nothing
    , _csgCacheSecurityGroupName = Nothing
    , _csgDescription            = Nothing
    , _csgEC2SecurityGroups      = mempty
    }

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

-- | The AWS account ID of the cache security group owner.
csgOwnerId :: Lens' CacheSecurityGroup (Maybe Text)
csgOwnerId = lens _csgOwnerId (\s a -> s { _csgOwnerId = a })

instance FromXML CacheSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroup"

instance ToQuery CacheSecurityGroup

data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter
    { _cntspAllowedValues               :: Maybe Text
    , _cntspCacheNodeTypeSpecificValues :: [CacheNodeTypeSpecificValue]
    , _cntspDataType                    :: Maybe Text
    , _cntspDescription                 :: Maybe Text
    , _cntspIsModifiable                :: Maybe Bool
    , _cntspMinimumEngineVersion        :: Maybe Text
    , _cntspParameterName               :: Maybe Text
    , _cntspSource                      :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'CacheNodeTypeSpecificParameter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cntspAllowedValues' @::@ 'Maybe' 'Text'
--
-- * 'cntspCacheNodeTypeSpecificValues' @::@ ['CacheNodeTypeSpecificValue']
--
-- * 'cntspDataType' @::@ 'Maybe' 'Text'
--
-- * 'cntspDescription' @::@ 'Maybe' 'Text'
--
-- * 'cntspIsModifiable' @::@ 'Maybe' 'Bool'
--
-- * 'cntspMinimumEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'cntspParameterName' @::@ 'Maybe' 'Text'
--
-- * 'cntspSource' @::@ 'Maybe' 'Text'
--
cacheNodeTypeSpecificParameter :: CacheNodeTypeSpecificParameter
cacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter
    { _cntspParameterName               = Nothing
    , _cntspDescription                 = Nothing
    , _cntspSource                      = Nothing
    , _cntspDataType                    = Nothing
    , _cntspAllowedValues               = Nothing
    , _cntspIsModifiable                = Nothing
    , _cntspMinimumEngineVersion        = Nothing
    , _cntspCacheNodeTypeSpecificValues = mempty
    }

-- | The valid range of values for the parameter.
cntspAllowedValues :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspAllowedValues =
    lens _cntspAllowedValues (\s a -> s { _cntspAllowedValues = a })

-- | A list of cache node types and their corresponding values for this
-- parameter.
cntspCacheNodeTypeSpecificValues :: Lens' CacheNodeTypeSpecificParameter [CacheNodeTypeSpecificValue]
cntspCacheNodeTypeSpecificValues =
    lens _cntspCacheNodeTypeSpecificValues
        (\s a -> s { _cntspCacheNodeTypeSpecificValues = a })

-- | The valid data type for the parameter.
cntspDataType :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDataType = lens _cntspDataType (\s a -> s { _cntspDataType = a })

-- | A description of the parameter.
cntspDescription :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDescription = lens _cntspDescription (\s a -> s { _cntspDescription = a })

-- | Indicates whether (true) or not (false) the parameter can be modified.
-- Some parameters have security or operational implications that prevent
-- them from being changed.
cntspIsModifiable :: Lens' CacheNodeTypeSpecificParameter (Maybe Bool)
cntspIsModifiable =
    lens _cntspIsModifiable (\s a -> s { _cntspIsModifiable = a })

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspMinimumEngineVersion =
    lens _cntspMinimumEngineVersion
        (\s a -> s { _cntspMinimumEngineVersion = a })

-- | The name of the parameter.
cntspParameterName :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspParameterName =
    lens _cntspParameterName (\s a -> s { _cntspParameterName = a })

-- | The source of the parameter value.
cntspSource :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspSource = lens _cntspSource (\s a -> s { _cntspSource = a })

instance FromXML CacheNodeTypeSpecificParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNodeTypeSpecificParameter"

instance ToQuery CacheNodeTypeSpecificParameter

data AZMode
    = CrossAz  -- ^ cross-az
    | SingleAz -- ^ single-az
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable AZMode

instance FromText AZMode where
    parser = match "cross-az"  CrossAz
         <|> match "single-az" SingleAz

instance ToText AZMode where
    toText = \case
        CrossAz  -> "cross-az"
        SingleAz -> "single-az"

instance FromXML AZMode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AZMode"

instance ToQuery AZMode

data CacheEngineVersion = CacheEngineVersion
    { _cevCacheEngineDescription        :: Maybe Text
    , _cevCacheEngineVersionDescription :: Maybe Text
    , _cevCacheParameterGroupFamily     :: Maybe Text
    , _cevEngine                        :: Maybe Text
    , _cevEngineVersion                 :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CacheEngineVersion' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cevCacheEngineDescription' @::@ 'Maybe' 'Text'
--
-- * 'cevCacheEngineVersionDescription' @::@ 'Maybe' 'Text'
--
-- * 'cevCacheParameterGroupFamily' @::@ 'Maybe' 'Text'
--
-- * 'cevEngine' @::@ 'Maybe' 'Text'
--
-- * 'cevEngineVersion' @::@ 'Maybe' 'Text'
--
cacheEngineVersion :: CacheEngineVersion
cacheEngineVersion = CacheEngineVersion
    { _cevEngine                        = Nothing
    , _cevEngineVersion                 = Nothing
    , _cevCacheParameterGroupFamily     = Nothing
    , _cevCacheEngineDescription        = Nothing
    , _cevCacheEngineVersionDescription = Nothing
    }

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

-- | The name of the cache parameter group family associated with this cache
-- engine.
cevCacheParameterGroupFamily :: Lens' CacheEngineVersion (Maybe Text)
cevCacheParameterGroupFamily =
    lens _cevCacheParameterGroupFamily
        (\s a -> s { _cevCacheParameterGroupFamily = a })

-- | The name of the cache engine.
cevEngine :: Lens' CacheEngineVersion (Maybe Text)
cevEngine = lens _cevEngine (\s a -> s { _cevEngine = a })

-- | The version number of the cache engine.
cevEngineVersion :: Lens' CacheEngineVersion (Maybe Text)
cevEngineVersion = lens _cevEngineVersion (\s a -> s { _cevEngineVersion = a })

instance FromXML CacheEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheEngineVersion"

instance ToQuery CacheEngineVersion

data ReplicationGroup = ReplicationGroup
    { _rgAutomaticFailover     :: Maybe Text
    , _rgDescription           :: Maybe Text
    , _rgMemberClusters        :: [Text]
    , _rgNodeGroups            :: [NodeGroup]
    , _rgPendingModifiedValues :: Maybe ReplicationGroupPendingModifiedValues
    , _rgReplicationGroupId    :: Maybe Text
    , _rgSnapshottingClusterId :: Maybe Text
    , _rgStatus                :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ReplicationGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rgAutomaticFailover' @::@ 'Maybe' 'Text'
--
-- * 'rgDescription' @::@ 'Maybe' 'Text'
--
-- * 'rgMemberClusters' @::@ ['Text']
--
-- * 'rgNodeGroups' @::@ ['NodeGroup']
--
-- * 'rgPendingModifiedValues' @::@ 'Maybe' 'ReplicationGroupPendingModifiedValues'
--
-- * 'rgReplicationGroupId' @::@ 'Maybe' 'Text'
--
-- * 'rgSnapshottingClusterId' @::@ 'Maybe' 'Text'
--
-- * 'rgStatus' @::@ 'Maybe' 'Text'
--
replicationGroup :: ReplicationGroup
replicationGroup = ReplicationGroup
    { _rgReplicationGroupId    = Nothing
    , _rgDescription           = Nothing
    , _rgStatus                = Nothing
    , _rgPendingModifiedValues = Nothing
    , _rgMemberClusters        = mempty
    , _rgNodeGroups            = mempty
    , _rgSnapshottingClusterId = Nothing
    , _rgAutomaticFailover     = Nothing
    }

-- | Indicates the status of automatic failover for this replication group.
rgAutomaticFailover :: Lens' ReplicationGroup (Maybe Text)
rgAutomaticFailover =
    lens _rgAutomaticFailover (\s a -> s { _rgAutomaticFailover = a })

-- | The description of the replication group.
rgDescription :: Lens' ReplicationGroup (Maybe Text)
rgDescription = lens _rgDescription (\s a -> s { _rgDescription = a })

-- | The names of all the cache clusters that are part of this replication
-- group.
rgMemberClusters :: Lens' ReplicationGroup [Text]
rgMemberClusters = lens _rgMemberClusters (\s a -> s { _rgMemberClusters = a })

-- | A single element list with information about the nodes in the replication
-- group.
rgNodeGroups :: Lens' ReplicationGroup [NodeGroup]
rgNodeGroups = lens _rgNodeGroups (\s a -> s { _rgNodeGroups = a })

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
rgPendingModifiedValues :: Lens' ReplicationGroup (Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues =
    lens _rgPendingModifiedValues (\s a -> s { _rgPendingModifiedValues = a })

-- | The identifier for the replication group.
rgReplicationGroupId :: Lens' ReplicationGroup (Maybe Text)
rgReplicationGroupId =
    lens _rgReplicationGroupId (\s a -> s { _rgReplicationGroupId = a })

-- | The cache cluster ID that is used as the daily snapshot source for the
-- replication group.
rgSnapshottingClusterId :: Lens' ReplicationGroup (Maybe Text)
rgSnapshottingClusterId =
    lens _rgSnapshottingClusterId (\s a -> s { _rgSnapshottingClusterId = a })

-- | The current state of this replication group - creating, available, etc.
rgStatus :: Lens' ReplicationGroup (Maybe Text)
rgStatus = lens _rgStatus (\s a -> s { _rgStatus = a })

instance FromXML ReplicationGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroup"

instance ToQuery ReplicationGroup

data RecurringCharge = RecurringCharge
    { _rcRecurringChargeAmount    :: Maybe Double
    , _rcRecurringChargeFrequency :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RecurringCharge' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcRecurringChargeAmount' @::@ 'Maybe' 'Double'
--
-- * 'rcRecurringChargeFrequency' @::@ 'Maybe' 'Text'
--
recurringCharge :: RecurringCharge
recurringCharge = RecurringCharge
    { _rcRecurringChargeAmount    = Nothing
    , _rcRecurringChargeFrequency = Nothing
    }

-- | The monetary amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount =
    lens _rcRecurringChargeAmount (\s a -> s { _rcRecurringChargeAmount = a })

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency =
    lens _rcRecurringChargeFrequency
        (\s a -> s { _rcRecurringChargeFrequency = a })

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge

data ReservedCacheNodesOffering = ReservedCacheNodesOffering
    { _rcnoCacheNodeType                :: Maybe Text
    , _rcnoDuration                     :: Maybe Int
    , _rcnoFixedPrice                   :: Maybe Double
    , _rcnoOfferingType                 :: Maybe Text
    , _rcnoProductDescription           :: Maybe Text
    , _rcnoRecurringCharges             :: [RecurringCharge]
    , _rcnoReservedCacheNodesOfferingId :: Maybe Text
    , _rcnoUsagePrice                   :: Maybe Double
    } deriving (Eq, Show, Generic)

-- | 'ReservedCacheNodesOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcnoCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'rcnoDuration' @::@ 'Maybe' 'Int'
--
-- * 'rcnoFixedPrice' @::@ 'Maybe' 'Double'
--
-- * 'rcnoOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'rcnoProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'rcnoRecurringCharges' @::@ ['RecurringCharge']
--
-- * 'rcnoReservedCacheNodesOfferingId' @::@ 'Maybe' 'Text'
--
-- * 'rcnoUsagePrice' @::@ 'Maybe' 'Double'
--
reservedCacheNodesOffering :: ReservedCacheNodesOffering
reservedCacheNodesOffering = ReservedCacheNodesOffering
    { _rcnoReservedCacheNodesOfferingId = Nothing
    , _rcnoCacheNodeType                = Nothing
    , _rcnoDuration                     = Nothing
    , _rcnoFixedPrice                   = Nothing
    , _rcnoUsagePrice                   = Nothing
    , _rcnoProductDescription           = Nothing
    , _rcnoOfferingType                 = Nothing
    , _rcnoRecurringCharges             = mempty
    }

-- | The cache node type for the reserved cache node.
rcnoCacheNodeType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoCacheNodeType =
    lens _rcnoCacheNodeType (\s a -> s { _rcnoCacheNodeType = a })

-- | The duration of the offering. in seconds.
rcnoDuration :: Lens' ReservedCacheNodesOffering (Maybe Int)
rcnoDuration = lens _rcnoDuration (\s a -> s { _rcnoDuration = a })

-- | The fixed price charged for this offering.
rcnoFixedPrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoFixedPrice = lens _rcnoFixedPrice (\s a -> s { _rcnoFixedPrice = a })

-- | The offering type.
rcnoOfferingType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoOfferingType = lens _rcnoOfferingType (\s a -> s { _rcnoOfferingType = a })

-- | The cache engine used by the offering.
rcnoProductDescription :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoProductDescription =
    lens _rcnoProductDescription (\s a -> s { _rcnoProductDescription = a })

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges :: Lens' ReservedCacheNodesOffering [RecurringCharge]
rcnoRecurringCharges =
    lens _rcnoRecurringCharges (\s a -> s { _rcnoRecurringCharges = a })

-- | A unique identifier for the reserved cache node offering.
rcnoReservedCacheNodesOfferingId :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoReservedCacheNodesOfferingId =
    lens _rcnoReservedCacheNodesOfferingId
        (\s a -> s { _rcnoReservedCacheNodesOfferingId = a })

-- | The hourly price charged for this offering.
rcnoUsagePrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoUsagePrice = lens _rcnoUsagePrice (\s a -> s { _rcnoUsagePrice = a })

instance FromXML ReservedCacheNodesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNodesOffering"

instance ToQuery ReservedCacheNodesOffering

data Endpoint = Endpoint
    { _eAddress :: Maybe Text
    , _ePort    :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'Endpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eAddress' @::@ 'Maybe' 'Text'
--
-- * 'ePort' @::@ 'Maybe' 'Int'
--
endpoint :: Endpoint
endpoint = Endpoint
    { _eAddress = Nothing
    , _ePort    = Nothing
    }

-- | The DNS hostname of the cache node.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\s a -> s { _eAddress = a })

-- | The port number that the cache engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\s a -> s { _ePort = a })

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint

data PendingModifiedValues = PendingModifiedValues
    { _pmvCacheNodeIdsToRemove :: [Text]
    , _pmvEngineVersion        :: Maybe Text
    , _pmvNumCacheNodes        :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'PendingModifiedValues' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmvCacheNodeIdsToRemove' @::@ ['Text']
--
-- * 'pmvEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'pmvNumCacheNodes' @::@ 'Maybe' 'Int'
--
pendingModifiedValues :: PendingModifiedValues
pendingModifiedValues = PendingModifiedValues
    { _pmvNumCacheNodes        = Nothing
    , _pmvCacheNodeIdsToRemove = mempty
    , _pmvEngineVersion        = Nothing
    }

-- | A list of cache node IDs that are being removed (or will be removed) from
-- the cache cluster. A node ID is a numeric identifier (0001, 0002, etc.).
pmvCacheNodeIdsToRemove :: Lens' PendingModifiedValues [Text]
pmvCacheNodeIdsToRemove =
    lens _pmvCacheNodeIdsToRemove (\s a -> s { _pmvCacheNodeIdsToRemove = a })

-- | The new cache engine version that the cache cluster will run.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\s a -> s { _pmvEngineVersion = a })

-- | The new number of cache nodes for the cache cluster.
pmvNumCacheNodes :: Lens' PendingModifiedValues (Maybe Int)
pmvNumCacheNodes = lens _pmvNumCacheNodes (\s a -> s { _pmvNumCacheNodes = a })

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues

newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage
    { _cpgnmCacheParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CacheParameterGroupNameMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgnmCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
cacheParameterGroupNameMessage :: CacheParameterGroupNameMessage
cacheParameterGroupNameMessage = CacheParameterGroupNameMessage
    { _cpgnmCacheParameterGroupName = Nothing
    }

-- | The name of the cache parameter group.
cpgnmCacheParameterGroupName :: Lens' CacheParameterGroupNameMessage (Maybe Text)
cpgnmCacheParameterGroupName =
    lens _cpgnmCacheParameterGroupName
        (\s a -> s { _cpgnmCacheParameterGroupName = a })

instance FromXML CacheParameterGroupNameMessage where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroupNameMessage"

instance ToQuery CacheParameterGroupNameMessage

data Parameter = Parameter
    { _pAllowedValues        :: Maybe Text
    , _pDataType             :: Maybe Text
    , _pDescription          :: Maybe Text
    , _pIsModifiable         :: Maybe Bool
    , _pMinimumEngineVersion :: Maybe Text
    , _pParameterName        :: Maybe Text
    , _pParameterValue       :: Maybe Text
    , _pSource               :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'Parameter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pAllowedValues' @::@ 'Maybe' 'Text'
--
-- * 'pDataType' @::@ 'Maybe' 'Text'
--
-- * 'pDescription' @::@ 'Maybe' 'Text'
--
-- * 'pIsModifiable' @::@ 'Maybe' 'Bool'
--
-- * 'pMinimumEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'pParameterName' @::@ 'Maybe' 'Text'
--
-- * 'pParameterValue' @::@ 'Maybe' 'Text'
--
-- * 'pSource' @::@ 'Maybe' 'Text'
--
parameter :: Parameter
parameter = Parameter
    { _pParameterName        = Nothing
    , _pParameterValue       = Nothing
    , _pDescription          = Nothing
    , _pSource               = Nothing
    , _pDataType             = Nothing
    , _pAllowedValues        = Nothing
    , _pIsModifiable         = Nothing
    , _pMinimumEngineVersion = Nothing
    }

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\s a -> s { _pAllowedValues = a })

-- | The valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\s a -> s { _pDataType = a })

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\s a -> s { _pDescription = a })

-- | Indicates whether (true) or not (false) the parameter can be modified.
-- Some parameters have security or operational implications that prevent
-- them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\s a -> s { _pIsModifiable = a })

-- | The earliest cache engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion =
    lens _pMinimumEngineVersion (\s a -> s { _pMinimumEngineVersion = a })

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\s a -> s { _pParameterName = a })

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\s a -> s { _pParameterValue = a })

-- | The source of the parameter.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\s a -> s { _pSource = a })

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

instance ToQuery Parameter
