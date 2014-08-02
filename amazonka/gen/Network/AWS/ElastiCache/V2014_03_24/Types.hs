{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.Types
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
module Network.AWS.ElastiCache.V2014_03_24.Types where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-03-24@) of the
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

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "elasticache"
        , _svcVersion  = "2014-03-24"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er ElastiCache)
deriving instance Generic (Er ElastiCache)

instance AWSError (Er ElastiCache) where
    awsError = const "ElastiCacheError"

instance ServiceError (Er ElastiCache) where
    serviceError    = ElastiCacheService
    clientError     = ElastiCacheClient
    serializerError = ElastiCacheSerializer

instance Exception (Er ElastiCache)

xmlOptions :: Tagged a XMLOptions
xmlOptions = Tagged def
    { xmlNamespace = Just "http://elasticache.amazonaws.com/doc/2014-03-24/"
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
    toQuery = genericToQuery def

-- | The Availability Zone associated with the subnet.
newtype AvailabilityZone = AvailabilityZone
    { _azName :: Maybe Text
      -- ^ The name of the availability zone.
    } deriving (Generic)

instance FromXML AvailabilityZone where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AvailabilityZone"

instance ToQuery AvailabilityZone where
    toQuery = genericToQuery def

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
newtype ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues
    { _rgpmvPrimaryClusterId :: Maybe Text
      -- ^ The primary cluster ID which will be applied immediately (if
      -- --apply-immediately was specified), or during the next
      -- maintenance window.
    } deriving (Generic)

instance FromXML ReplicationGroupPendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroupPendingModifiedValues"

instance ToQuery ReplicationGroupPendingModifiedValues where
    toQuery = genericToQuery def

-- | Contains all of the attributes of a specific cache cluster.
data CacheCluster = CacheCluster
    { _ccEngineVersion :: Maybe Text
      -- ^ The version of the cache engine version that is used in this
      -- cache cluster.
    , _ccCacheNodeType :: Maybe Text
      -- ^ The name of the compute and memory capacity node type for the
      -- cache cluster.
    , _ccCacheNodes :: [CacheNode]
      -- ^ A list of cache nodes that are members of the cache cluster.
    , _ccCacheClusterCreateTime :: Maybe ISO8601
      -- ^ The date and time when the cache cluster was created.
    , _ccAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ If true, then minor version patches are applied automatically; if
      -- false, then automatic minor version patches are disabled.
    , _ccSecurityGroups :: [SecurityGroupMembership]
      -- ^ A list of VPC Security Groups associated with the cache cluster.
    , _ccNotificationConfiguration :: Maybe NotificationConfiguration
      -- ^ Describes a notification topic and its status. Notification
      -- topics are used for publishing ElastiCache events to subscribers
      -- using Amazon Simple Notification Service (SNS).
    , _ccSnapshotWindow :: Maybe Text
      -- ^ The daily time range (in UTC) during which ElastiCache will begin
      -- taking a daily snapshot of your cache cluster. Example:
      -- 05:00-09:00.
    , _ccCacheClusterId :: Maybe Text
      -- ^ The user-supplied identifier of the cache cluster. This is a
      -- unique key that identifies a cache cluster.
    , _ccConfigurationEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to
      -- connect to a cache node.
    , _ccEngine :: Maybe Text
      -- ^ The name of the cache engine (memcached or redis) to be used for
      -- this cache cluster.
    , _ccCacheSecurityGroups :: [CacheSecurityGroupMembership]
      -- ^ A list of cache security group elements, composed of name and
      -- status sub-elements.
    , _ccClientDownloadLandingPage :: Maybe Text
      -- ^ The URL of the web page where you can download the latest
      -- ElastiCache client library.
    , _ccPreferredMaintenanceWindow :: Maybe Text
      -- ^ The time range (in UTC) during which weekly system maintenance
      -- can occur.
    , _ccCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group associated with the cache
      -- cluster.
    , _ccPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the cache cluster is
      -- located.
    , _ccCacheParameterGroup :: Maybe CacheParameterGroupStatus
      -- ^ The status of the cache parameter group.
    , _ccCacheClusterStatus :: Maybe Text
      -- ^ The current state of this cache cluster - creating, available,
      -- etc.
    , _ccSnapshotRetentionLimit :: Maybe Integer
      -- ^ The number of days for which ElastiCache will retain automatic
      -- cache cluster snapshots before deleting them. For example, if you
      -- set SnapshotRetentionLimit to 5, then a snapshot that was taken
      -- today will be retained for 5 days before being deleted.
    , _ccReplicationGroupId :: Maybe Text
      -- ^ The replication group to which this cache cluster belongs. If
      -- this field is empty, the cache cluster is not associated with any
      -- replication group.
    , _ccPendingModifiedValues :: Maybe PendingModifiedValues
      -- ^ A group of settings that will be applied to the cache cluster in
      -- the future, or that are currently being applied.
    , _ccNumCacheNodes :: Maybe Integer
      -- ^ The number of cache nodes in the cache cluster.
    } deriving (Generic)

instance FromXML CacheCluster where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheCluster"

-- | Provides all of the details about a particular cache engine version.
data CacheEngineVersion = CacheEngineVersion
    { _cevEngineVersion :: Maybe Text
      -- ^ The version number of the cache engine.
    , _cevCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of the cache parameter group family associated with this
      -- cache engine.
    , _cevCacheEngineDescription :: Maybe Text
      -- ^ The description of the cache engine.
    , _cevEngine :: Maybe Text
      -- ^ The name of the cache engine.
    , _cevCacheEngineVersionDescription :: Maybe Text
      -- ^ The description of the cache engine version.
    } deriving (Generic)

instance FromXML CacheEngineVersion where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheEngineVersion"

-- | Represents an individual cache node within a cache cluster. Each cache node
-- runs its own instance of the cluster's protocol-compliant caching software
-- - either Memcached or Redis.
data CacheNode = CacheNode
    { _cnSourceCacheNodeId :: Maybe Text
      -- ^ The ID of the primary node to which this read replica node is
      -- synchronized. If this field is empty, then this node is not
      -- associated with a primary cache cluster.
    , _cnParameterGroupStatus :: Maybe Text
      -- ^ The status of the parameter group applied to this cache node.
    , _cnCacheNodeCreateTime :: Maybe ISO8601
      -- ^ The date and time when the cache node was created.
    , _cnCacheNodeId :: Maybe Text
      -- ^ The cache node identifier. A node ID is a numeric identifier
      -- (0001, 0002, etc.). The combination of cluster ID and node ID
      -- uniquely identifies every cache node used in a customer's AWS
      -- account.
    , _cnCacheNodeStatus :: Maybe Text
      -- ^ The current state of this cache node.
    , _cnEndpoint :: Maybe Endpoint
      -- ^ The hostname and IP address for connecting to this cache node.
    } deriving (Generic)

instance FromXML CacheNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNode"

instance ToQuery CacheNode where
    toQuery = genericToQuery def

-- | A parameter that has a different value for each cache node type it is
-- applied to. For example, in a Redis cache cluster, a cache.m1.large cache
-- node type would have a larger maxmemory value than a cache.m1.small type.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter
    { _cntspCacheNodeTypeSpecificValues :: [CacheNodeTypeSpecificValue]
      -- ^ A list of cache node types and their corresponding values for
      -- this parameter.
    , _cntspMinimumEngineVersion :: Maybe Text
      -- ^ The earliest cache engine version to which the parameter can
      -- apply.
    , _cntspSource :: Maybe Text
      -- ^ The source of the parameter value.
    , _cntspIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be
      -- modified. Some parameters have security or operational
      -- implications that prevent them from being changed.
    , _cntspDataType :: Maybe Text
      -- ^ The valid data type for the parameter.
    , _cntspAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , _cntspParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , _cntspDescription :: Maybe Text
      -- ^ A description of the parameter.
    } deriving (Generic)

instance FromXML CacheNodeTypeSpecificParameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNodeTypeSpecificParameter"

-- | A value that applies only to a certain cache node type.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue
    { _cntsvCacheNodeType :: Maybe Text
      -- ^ The cache node type for which this value applies.
    , _cntsvValue :: Maybe Text
      -- ^ The value for the cache node type.
    } deriving (Generic)

instance FromXML CacheNodeTypeSpecificValue where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheNodeTypeSpecificValue"

instance ToQuery CacheNodeTypeSpecificValue where
    toQuery = genericToQuery def

-- | Represents the output of a CreateCacheParameterGroup operation.
data CacheParameterGroup = CacheParameterGroup
    { _cpgCacheParameterGroupFamily :: Maybe Text
      -- ^ The name of the cache parameter group family that this cache
      -- parameter group is compatible with.
    , _cpgCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    , _cpgDescription :: Maybe Text
      -- ^ The description for this cache parameter group.
    } deriving (Generic)

instance FromXML CacheParameterGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroup"

-- | The status of the cache parameter group.
data CacheParameterGroupStatus = CacheParameterGroupStatus
    { _cpgsCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    , _cpgsCacheNodeIdsToReboot :: [Text]
      -- ^ A list of the cache node IDs which need to be rebooted for
      -- parameter changes to be applied. A node ID is a numeric
      -- identifier (0001, 0002, etc.).
    , _cpgsParameterApplyStatus :: Maybe Text
      -- ^ The status of parameter updates.
    } deriving (Generic)

instance FromXML CacheParameterGroupStatus where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheParameterGroupStatus"

instance ToQuery CacheParameterGroupStatus where
    toQuery = genericToQuery def

-- | Represents the output of one of the following operations:
-- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
-- RevokeCacheSecurityGroupIngress.
data CacheSecurityGroup = CacheSecurityGroup
    { _csgCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group.
    , _csgOwnerId :: Maybe Text
      -- ^ The AWS account ID of the cache security group owner.
    , _csgEC2SecurityGroups :: [EC2SecurityGroup]
      -- ^ A list of Amazon EC2 security groups that are associated with
      -- this cache security group.
    , _csgDescription :: Maybe Text
      -- ^ The description of the cache security group.
    } deriving (Generic)

instance FromXML CacheSecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroup"

-- | Represents a cache cluster's status within a particular cache security
-- group.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership
    { _csgmStatus :: Maybe Text
      -- ^ The membership status in the cache security group. The status
      -- changes when a cache security group is modified, or when the
      -- cache security groups assigned to a cache cluster are modified.
    , _csgmCacheSecurityGroupName :: Maybe Text
      -- ^ The name of the cache security group.
    } deriving (Generic)

instance FromXML CacheSecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSecurityGroup"

instance ToQuery CacheSecurityGroupMembership where
    toQuery = genericToQuery def

-- | Represents the output of one of the following operations:
-- CreateCacheSubnetGroup ModifyCacheSubnetGroup.
data CacheSubnetGroup = CacheSubnetGroup
    { _csiVpcId :: Maybe Text
      -- ^ The Amazon Virtual Private Cloud identifier (VPC ID) of the cache
      -- subnet group.
    , _csiSubnets :: [Subnet]
      -- ^ A list of subnets associated with the cache subnet group.
    , _csiCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group.
    , _csiCacheSubnetGroupDescription :: Maybe Text
      -- ^ The description of the cache subnet group.
    } deriving (Generic)

instance FromXML CacheSubnetGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CacheSubnetGroup"

-- | Provides ownership and status information for an Amazon EC2 security group.
data EC2SecurityGroup = EC2SecurityGroup
    { _ecsgStatus :: Maybe Text
      -- ^ The status of the Amazon EC2 security group.
    , _ecsgEC2SecurityGroupOwnerId :: Maybe Text
      -- ^ The AWS account ID of the Amazon EC2 security group owner.
    , _ecsgEC2SecurityGroupName :: Maybe Text
      -- ^ The name of the Amazon EC2 security group.
    } deriving (Generic)

instance FromXML EC2SecurityGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EC2SecurityGroup"

instance ToQuery EC2SecurityGroup where
    toQuery = genericToQuery def

-- | Represents the information required for client programs to connect to a
-- cache node.
data Endpoint = Endpoint
    { _eAddress :: Maybe Text
      -- ^ The DNS hostname of the cache node.
    , _ePort :: Maybe Integer
      -- ^ The port number that the cache engine is listening on.
    } deriving (Generic)

instance FromXML Endpoint where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Endpoint"

instance ToQuery Endpoint where
    toQuery = genericToQuery def

-- | Represents the output of a DescribeEngineDefaultParameters operation.
data EngineDefaults = EngineDefaults
    { _edCacheParameterGroupFamily :: Maybe Text
      -- ^ Specifies the name of the cache parameter group family to which
      -- the engine default parameters apply.
    , _edCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
      -- ^ A list of parameters specific to a particular cache node type.
      -- Each element in the list contains detailed information about one
      -- parameter.
    , _edMarker :: Maybe Text
      -- ^ Provides an identifier to allow retrieval of paginated results.
    , _edParameters :: [Parameter]
      -- ^ Contains a list of engine default parameters.
    } deriving (Generic)

instance FromXML EngineDefaults where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EngineDefaults"

-- | Represents a single occurrence of something interesting within the system.
-- Some examples of events are creating a cache cluster, adding or removing a
-- cache node, or rebooting a node.
data Event = Event
    { _euSourceType :: Maybe SourceType
      -- ^ Specifies the origin of this event - a cache cluster, a parameter
      -- group, a security group, etc.
    , _euSourceIdentifier :: Maybe Text
      -- ^ The identifier for the source of the event. For example, if the
      -- event occurred at the cache cluster level, the identifier would
      -- be the name of the cache cluster.
    , _euDate :: Maybe ISO8601
      -- ^ The date and time when the event occurred.
    , _euMessage :: Maybe Text
      -- ^ The text of the event.
    } deriving (Generic)

instance FromXML Event where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Event"

-- | Represents a collection of cache nodes in a replication group.
data NodeGroup = NodeGroup
    { _ngStatus :: Maybe Text
      -- ^ The current state of this replication group - creating,
      -- available, etc.
    , _ngPrimaryEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to
      -- connect to a cache node.
    , _ngNodeGroupMembers :: [NodeGroupMember]
      -- ^ A list containing information about individual nodes within the
      -- node group.
    , _ngNodeGroupId :: Maybe Text
      -- ^ The identifier for the node group. A replication group contains
      -- only one node group; therefore, the node group ID is 0001.
    } deriving (Generic)

instance FromXML NodeGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroup"

instance ToQuery NodeGroup where
    toQuery = genericToQuery def

-- | Represents a single node within a node group.
data NodeGroupMember = NodeGroupMember
    { _ngmCacheClusterId :: Maybe Text
      -- ^ The ID of the cache cluster to which the node belongs.
    , _ngmCacheNodeId :: Maybe Text
      -- ^ The ID of the node within its cache cluster. A node ID is a
      -- numeric identifier (0001, 0002, etc.).
    , _ngmPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the node is located.
    , _ngmCurrentRole :: Maybe Text
      -- ^ The role that is currently assigned to the node - primary or
      -- replica.
    , _ngmReadEndpoint :: Maybe Endpoint
      -- ^ Represents the information required for client programs to
      -- connect to a cache node.
    } deriving (Generic)

instance FromXML NodeGroupMember where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeGroupMember"

instance ToQuery NodeGroupMember where
    toQuery = genericToQuery def

-- | Represents an individual cache node in a snapshot of a cache cluster.
data NodeSnapshot = NodeSnapshot
    { _nsCacheNodeCreateTime :: Maybe ISO8601
      -- ^ The date and time when the cache node was created in the source
      -- cache cluster.
    , _nsCacheNodeId :: Maybe Text
      -- ^ The cache node identifier for the node in the source cache
      -- cluster.
    , _nsSnapshotCreateTime :: Maybe ISO8601
      -- ^ The date and time when the source node's metadata and cache data
      -- set was obtained for the snapshot.
    , _nsCacheSize :: Maybe Text
      -- ^ The size of the cache on the source cache node.
    } deriving (Generic)

instance FromXML NodeSnapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NodeSnapshot"

instance ToQuery NodeSnapshot where
    toQuery = genericToQuery def

-- | Describes a notification topic and its status. Notification topics are used
-- for publishing ElastiCache events to subscribers using Amazon Simple
-- Notification Service (SNS).
data NotificationConfiguration = NotificationConfiguration
    { _ncTopicStatus :: Maybe Text
      -- ^ The current state of the topic.
    , _ncTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) that identifies the topic.
    } deriving (Generic)

instance FromXML NotificationConfiguration where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "NotificationConfiguration"

instance ToQuery NotificationConfiguration where
    toQuery = genericToQuery def

-- | Describes an individual setting that controls some aspect of ElastiCache
-- behavior.
data Parameter = Parameter
    { _prParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    , _prMinimumEngineVersion :: Maybe Text
      -- ^ The earliest cache engine version to which the parameter can
      -- apply.
    , _prSource :: Maybe Text
      -- ^ The source of the parameter.
    , _prIsModifiable :: Maybe Bool
      -- ^ Indicates whether (true) or not (false) the parameter can be
      -- modified. Some parameters have security or operational
      -- implications that prevent them from being changed.
    , _prDataType :: Maybe Text
      -- ^ The valid data type for the parameter.
    , _prAllowedValues :: Maybe Text
      -- ^ The valid range of values for the parameter.
    , _prParameterName :: Maybe Text
      -- ^ The name of the parameter.
    , _prDescription :: Maybe Text
      -- ^ A description of the parameter.
    } deriving (Generic)

instance FromXML Parameter where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Parameter"

-- | Describes a name-value pair that is used to update the value of a
-- parameter.
data ParameterNameValue = ParameterNameValue
    { _pnvParameterValue :: Maybe Text
      -- ^ The value of the parameter.
    , _pnvParameterName :: Maybe Text
      -- ^ The name of the parameter.
    } deriving (Generic)

instance ToQuery ParameterNameValue where
    toQuery = genericToQuery def

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
data PendingModifiedValues = PendingModifiedValues
    { _pmvEngineVersion :: Maybe Text
      -- ^ The new cache engine version that the cache cluster will run.
    , _pmvCacheNodeIdsToRemove :: [Text]
      -- ^ A list of cache node IDs that are being removed (or will be
      -- removed) from the cache cluster. A node ID is a numeric
      -- identifier (0001, 0002, etc.).
    , _pmvNumCacheNodes :: Maybe Integer
      -- ^ The new number of cache nodes for the cache cluster.
    } deriving (Generic)

instance FromXML PendingModifiedValues where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PendingModifiedValues"

instance ToQuery PendingModifiedValues where
    toQuery = genericToQuery def

-- | Contains the specific price and frequency of a recurring charges for a
-- reserved cache node, or for a reserved cache node offering.
data RecurringCharge = RecurringCharge
    { _rcRecurringChargeFrequency :: Maybe Text
      -- ^ The frequency of the recurring charge.
    , _rcRecurringChargeAmount :: Maybe Double
      -- ^ The monetary amount of the recurring charge.
    } deriving (Generic)

instance FromXML RecurringCharge where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "RecurringCharge"

instance ToQuery RecurringCharge where
    toQuery = genericToQuery def

-- | Contains all of the attributes of a specific replication group.
data ReplicationGroup = ReplicationGroup
    { _rgStatus :: Maybe Text
      -- ^ The current state of this replication group - creating,
      -- available, etc.
    , _rgNodeGroups :: [NodeGroup]
      -- ^ A single element list with information about the nodes in the
      -- replication group.
    , _rgSnapshottingClusterId :: Maybe Text
      -- ^ The cache cluster ID that is used as the daily snapshot source
      -- for the replication group.
    , _rgMemberClusters :: [Text]
      -- ^ The names of all the cache clusters that are part of this
      -- replication group.
    , _rgDescription :: Maybe Text
      -- ^ The description of the replication group.
    , _rgReplicationGroupId :: Maybe Text
      -- ^ The identifier for the replication group.
    , _rgPendingModifiedValues :: Maybe ReplicationGroupPendingModifiedValues
      -- ^ A group of settings to be applied to the replication group,
      -- either immediately or during the next maintenance window.
    } deriving (Generic)

instance FromXML ReplicationGroup where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReplicationGroup"

-- | Represents the output of a PurchaseReservedCacheNodesOffering operation.
data ReservedCacheNode = ReservedCacheNode
    { _rcnCacheNodeType :: Maybe Text
      -- ^ The cache node type for the reserved cache nodes.
    , _rcnState :: Maybe Text
      -- ^ The state of the reserved cache node.
    , _rcnStartTime :: Maybe ISO8601
      -- ^ The time the reservation started.
    , _rcnProductDescription :: Maybe Text
      -- ^ The description of the reserved cache node.
    , _rcnCacheNodeCount :: Maybe Integer
      -- ^ The number of cache nodes that have been reserved.
    , _rcnReservedCacheNodeId :: Maybe Text
      -- ^ The unique identifier for the reservation.
    , _rcnRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved cache node.
    , _rcnOfferingType :: Maybe Text
      -- ^ The offering type of this reserved cache node.
    , _rcnUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this reserved cache node.
    , _rcnFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this reserved cache node.
    , _rcnDuration :: Maybe Integer
      -- ^ The duration of the reservation in seconds.
    , _rcnReservedCacheNodesOfferingId :: Maybe Text
      -- ^ The offering identifier.
    } deriving (Generic)

instance FromXML ReservedCacheNode where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNode"

-- | Describes all of the attributes of a reserved cache node offering.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering
    { _rcnoCacheNodeType :: Maybe Text
      -- ^ The cache node type for the reserved cache node.
    , _rcnoProductDescription :: Maybe Text
      -- ^ The cache engine used by the offering.
    , _rcnoRecurringCharges :: [RecurringCharge]
      -- ^ The recurring price charged to run this reserved cache node.
    , _rcnoOfferingType :: Maybe Text
      -- ^ The offering type.
    , _rcnoUsagePrice :: Maybe Double
      -- ^ The hourly price charged for this offering.
    , _rcnoFixedPrice :: Maybe Double
      -- ^ The fixed price charged for this offering.
    , _rcnoDuration :: Maybe Integer
      -- ^ The duration of the offering. in seconds.
    , _rcnoReservedCacheNodesOfferingId :: Maybe Text
      -- ^ A unique identifier for the reserved cache node offering.
    } deriving (Generic)

instance FromXML ReservedCacheNodesOffering where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ReservedCacheNodesOffering"

-- | Represents a single cache security group and its status..
data SecurityGroupMembership = SecurityGroupMembership
    { _sgmStatus :: Maybe Text
      -- ^ The status of the cache security group membership. The status
      -- changes whenever a cache security group is modified, or when the
      -- cache security groups assigned to a cache cluster are modified.
    , _sgmSecurityGroupId :: Maybe Text
      -- ^ The identifier of the cache security group.
    } deriving (Generic)

instance FromXML SecurityGroupMembership where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SecurityGroupMembership"

instance ToQuery SecurityGroupMembership where
    toQuery = genericToQuery def

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
data Snapshot = Snapshot
    { _ssssssssssssssstEngineVersion :: Maybe Text
      -- ^ The version of the cache engine version that is used by the
      -- source cache cluster.
    , _ssssssssssssssstCacheNodeType :: Maybe Text
      -- ^ The name of the compute and memory capacity node type for the
      -- source cache cluster.
    , _ssssssssssssssstCacheClusterCreateTime :: Maybe ISO8601
      -- ^ The date and time when the source cache cluster was created.
    , _ssssssssssssssstAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ For the source cache cluster, indicates whether minor version
      -- patches are applied automatically (true) or not (false).
    , _ssssssssssssssstCacheParameterGroupName :: Maybe Text
      -- ^ The cache parameter group that is associated with the source
      -- cache cluster.
    , _ssssssssssssssstVpcId :: Maybe Text
      -- ^ The Amazon Virtual Private Cloud identifier (VPC ID) of the cache
      -- subnet group for the source cache cluster.
    , _ssssssssssssssstSnapshotStatus :: Maybe Text
      -- ^ The status of the snapshot. Valid values: creating | available |
      -- restoring | deleting.
    , _ssssssssssssssstSnapshotWindow :: Maybe Text
      -- ^ The daily time range during which ElastiCache takes daily
      -- snapshots of the source cache cluster.
    , _ssssssssssssssstCacheClusterId :: Maybe Text
      -- ^ The user-supplied identifier of the source cache cluster.
    , _ssssssssssssssstEngine :: Maybe Text
      -- ^ The name of the cache engine (memcached or redis) used by the
      -- source cache cluster.
    , _ssssssssssssssstPreferredMaintenanceWindow :: Maybe Text
      -- ^ The time range (in UTC) during which weekly system maintenance
      -- can occur on the source cache cluster.
    , _ssssssssssssssstTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) for the topic used by the source
      -- cache cluster for publishing notifications.
    , _ssssssssssssssstNodeSnapshots :: [NodeSnapshot]
      -- ^ A list of the cache cluster nodes in the source cache cluster.
    , _ssssssssssssssstCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group associated with the source
      -- cache cluster.
    , _ssssssssssssssstPreferredAvailabilityZone :: Maybe Text
      -- ^ The name of the Availability Zone in which the source cache
      -- cluster is located.
    , _ssssssssssssssstSnapshotRetentionLimit :: Maybe Integer
      -- ^ For an automatic snapshot, the number of days for which
      -- ElastiCache will retain the snapshot before deleting it. For
      -- manual snapshots, this field reflects the SnapshotRetentionLimit
      -- for the source cache cluster when the snapshot was created. This
      -- field is otherwise ignored: Manual snapshots do not expire, and
      -- can only be deleted using the DeleteSnapshot action.
    , _ssssssssssssssstSnapshotName :: Maybe Text
      -- ^ The name of a snapshot. For an automatic snapshot, the name is
      -- system-generated; for a manual snapshot, this is the
      -- user-provided name.
    , _ssssssssssssssstNumCacheNodes :: Maybe Integer
      -- ^ The number of cache nodes in the source cache cluster.
    , _ssssssssssssssstPort :: Maybe Integer
      -- ^ The port number used by each cache nodes in the source cache
      -- cluster.
    , _ssssssssssssssstSnapshotSource :: Maybe Text
      -- ^ Indicates whether the snapshot is from an automatic backup
      -- (automated) or was created manually (manual).
    } deriving (Generic)

instance FromXML Snapshot where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Snapshot"

-- | Represents the subnet associated with a cache cluster. This parameter
-- refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and
-- used with ElastiCache.
data Subnet = Subnet
    { _ssssssssssssswSubnetIdentifier :: Maybe Text
      -- ^ The unique identifier for the subnet.
    , _ssssssssssssswSubnetAvailabilityZone :: Maybe AvailabilityZone
      -- ^ The Availability Zone associated with the subnet.
    } deriving (Generic)

instance FromXML Subnet where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "Subnet"

instance ToQuery Subnet where
    toQuery = genericToQuery def
