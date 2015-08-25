{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Product
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.Product where

import           Network.AWS.ElastiCache.Types.Sum
import           Network.AWS.Prelude

-- | Describes an Availability Zone in which the cache cluster is launched.
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
    { _azName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName'
availabilityZone
    :: AvailabilityZone
availabilityZone =
    AvailabilityZone'
    { _azName = Nothing
    }

-- | The name of the Availability Zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a});

instance FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' <$> (x .@? "Name")

-- | Contains all of the attributes of a specific cache cluster.
--
-- /See:/ 'cacheCluster' smart constructor.
data CacheCluster = CacheCluster'
    { _ccCacheNodeType              :: !(Maybe Text)
    , _ccEngineVersion              :: !(Maybe Text)
    , _ccCacheNodes                 :: !(Maybe [CacheNode])
    , _ccCacheClusterCreateTime     :: !(Maybe ISO8601)
    , _ccAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _ccSecurityGroups             :: !(Maybe [SecurityGroupMembership])
    , _ccNotificationConfiguration  :: !(Maybe NotificationConfiguration)
    , _ccSnapshotWindow             :: !(Maybe Text)
    , _ccCacheClusterId             :: !(Maybe Text)
    , _ccConfigurationEndpoint      :: !(Maybe Endpoint)
    , _ccEngine                     :: !(Maybe Text)
    , _ccCacheSecurityGroups        :: !(Maybe [CacheSecurityGroupMembership])
    , _ccClientDownloadLandingPage  :: !(Maybe Text)
    , _ccPreferredMaintenanceWindow :: !(Maybe Text)
    , _ccCacheSubnetGroupName       :: !(Maybe Text)
    , _ccCacheClusterStatus         :: !(Maybe Text)
    , _ccPreferredAvailabilityZone  :: !(Maybe Text)
    , _ccCacheParameterGroup        :: !(Maybe CacheParameterGroupStatus)
    , _ccSnapshotRetentionLimit     :: !(Maybe Int)
    , _ccReplicationGroupId         :: !(Maybe Text)
    , _ccPendingModifiedValues      :: !(Maybe PendingModifiedValues)
    , _ccNumCacheNodes              :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccCacheNodeType'
--
-- * 'ccEngineVersion'
--
-- * 'ccCacheNodes'
--
-- * 'ccCacheClusterCreateTime'
--
-- * 'ccAutoMinorVersionUpgrade'
--
-- * 'ccSecurityGroups'
--
-- * 'ccNotificationConfiguration'
--
-- * 'ccSnapshotWindow'
--
-- * 'ccCacheClusterId'
--
-- * 'ccConfigurationEndpoint'
--
-- * 'ccEngine'
--
-- * 'ccCacheSecurityGroups'
--
-- * 'ccClientDownloadLandingPage'
--
-- * 'ccPreferredMaintenanceWindow'
--
-- * 'ccCacheSubnetGroupName'
--
-- * 'ccCacheClusterStatus'
--
-- * 'ccPreferredAvailabilityZone'
--
-- * 'ccCacheParameterGroup'
--
-- * 'ccSnapshotRetentionLimit'
--
-- * 'ccReplicationGroupId'
--
-- * 'ccPendingModifiedValues'
--
-- * 'ccNumCacheNodes'
cacheCluster
    :: CacheCluster
cacheCluster =
    CacheCluster'
    { _ccCacheNodeType = Nothing
    , _ccEngineVersion = Nothing
    , _ccCacheNodes = Nothing
    , _ccCacheClusterCreateTime = Nothing
    , _ccAutoMinorVersionUpgrade = Nothing
    , _ccSecurityGroups = Nothing
    , _ccNotificationConfiguration = Nothing
    , _ccSnapshotWindow = Nothing
    , _ccCacheClusterId = Nothing
    , _ccConfigurationEndpoint = Nothing
    , _ccEngine = Nothing
    , _ccCacheSecurityGroups = Nothing
    , _ccClientDownloadLandingPage = Nothing
    , _ccPreferredMaintenanceWindow = Nothing
    , _ccCacheSubnetGroupName = Nothing
    , _ccCacheClusterStatus = Nothing
    , _ccPreferredAvailabilityZone = Nothing
    , _ccCacheParameterGroup = Nothing
    , _ccSnapshotRetentionLimit = Nothing
    , _ccReplicationGroupId = Nothing
    , _ccPendingModifiedValues = Nothing
    , _ccNumCacheNodes = Nothing
    }

-- | The name of the compute and memory capacity node type for the cache
-- cluster.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: 'cache.t2.micro', 'cache.t2.small',
--         'cache.t2.medium', 'cache.m3.medium', 'cache.m3.large',
--         'cache.m3.xlarge', 'cache.m3.2xlarge'
--     -   Previous generation: 'cache.t1.micro', 'cache.m1.small',
--         'cache.m1.medium', 'cache.m1.large', 'cache.m1.xlarge'
-- -   Compute optimized: 'cache.c1.xlarge'
-- -   Memory optimized
--     -   Current generation: 'cache.r3.large', 'cache.r3.xlarge',
--         'cache.r3.2xlarge', 'cache.r3.4xlarge', 'cache.r3.8xlarge'
--     -   Previous generation: 'cache.m2.xlarge', 'cache.m2.2xlarge',
--         'cache.m2.4xlarge'
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
ccCacheNodeType :: Lens' CacheCluster (Maybe Text)
ccCacheNodeType = lens _ccCacheNodeType (\ s a -> s{_ccCacheNodeType = a});

-- | The version of the cache engine version that is used in this cache
-- cluster.
ccEngineVersion :: Lens' CacheCluster (Maybe Text)
ccEngineVersion = lens _ccEngineVersion (\ s a -> s{_ccEngineVersion = a});

-- | A list of cache nodes that are members of the cache cluster.
ccCacheNodes :: Lens' CacheCluster [CacheNode]
ccCacheNodes = lens _ccCacheNodes (\ s a -> s{_ccCacheNodes = a}) . _Default . _Coerce;

-- | The date and time when the cache cluster was created.
ccCacheClusterCreateTime :: Lens' CacheCluster (Maybe UTCTime)
ccCacheClusterCreateTime = lens _ccCacheClusterCreateTime (\ s a -> s{_ccCacheClusterCreateTime = a}) . mapping _Time;

-- | This parameter is currently disabled.
ccAutoMinorVersionUpgrade :: Lens' CacheCluster (Maybe Bool)
ccAutoMinorVersionUpgrade = lens _ccAutoMinorVersionUpgrade (\ s a -> s{_ccAutoMinorVersionUpgrade = a});

-- | A list of VPC Security Groups associated with the cache cluster.
ccSecurityGroups :: Lens' CacheCluster [SecurityGroupMembership]
ccSecurityGroups = lens _ccSecurityGroups (\ s a -> s{_ccSecurityGroups = a}) . _Default . _Coerce;

-- | Undocumented member.
ccNotificationConfiguration :: Lens' CacheCluster (Maybe NotificationConfiguration)
ccNotificationConfiguration = lens _ccNotificationConfiguration (\ s a -> s{_ccNotificationConfiguration = a});

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your cache cluster.
--
-- Example: '05:00-09:00'
ccSnapshotWindow :: Lens' CacheCluster (Maybe Text)
ccSnapshotWindow = lens _ccSnapshotWindow (\ s a -> s{_ccSnapshotWindow = a});

-- | The user-supplied identifier of the cache cluster. This identifier is a
-- unique key that identifies a cache cluster.
ccCacheClusterId :: Lens' CacheCluster (Maybe Text)
ccCacheClusterId = lens _ccCacheClusterId (\ s a -> s{_ccCacheClusterId = a});

-- | Undocumented member.
ccConfigurationEndpoint :: Lens' CacheCluster (Maybe Endpoint)
ccConfigurationEndpoint = lens _ccConfigurationEndpoint (\ s a -> s{_ccConfigurationEndpoint = a});

-- | The name of the cache engine (/memcached/ or /redis/) to be used for
-- this cache cluster.
ccEngine :: Lens' CacheCluster (Maybe Text)
ccEngine = lens _ccEngine (\ s a -> s{_ccEngine = a});

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
ccCacheSecurityGroups :: Lens' CacheCluster [CacheSecurityGroupMembership]
ccCacheSecurityGroups = lens _ccCacheSecurityGroups (\ s a -> s{_ccCacheSecurityGroups = a}) . _Default . _Coerce;

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
ccClientDownloadLandingPage :: Lens' CacheCluster (Maybe Text)
ccClientDownloadLandingPage = lens _ccClientDownloadLandingPage (\ s a -> s{_ccClientDownloadLandingPage = a});

-- | Specifies the weekly time range during which maintenance on the cache
-- cluster is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for 'ddd' are:
--
-- -   'sun'
-- -   'mon'
-- -   'tue'
-- -   'wed'
-- -   'thu'
-- -   'fri'
-- -   'sat'
--
-- Example: 'sun:05:00-sun:09:00'
ccPreferredMaintenanceWindow :: Lens' CacheCluster (Maybe Text)
ccPreferredMaintenanceWindow = lens _ccPreferredMaintenanceWindow (\ s a -> s{_ccPreferredMaintenanceWindow = a});

-- | The name of the cache subnet group associated with the cache cluster.
ccCacheSubnetGroupName :: Lens' CacheCluster (Maybe Text)
ccCacheSubnetGroupName = lens _ccCacheSubnetGroupName (\ s a -> s{_ccCacheSubnetGroupName = a});

-- | The current state of this cache cluster, one of the following values:
-- /available/, /creating/, /deleted/, /deleting/, /incompatible-network/,
-- /modifying/, /rebooting cache cluster nodes/, /restore-failed/, or
-- /snapshotting/.
ccCacheClusterStatus :: Lens' CacheCluster (Maybe Text)
ccCacheClusterStatus = lens _ccCacheClusterStatus (\ s a -> s{_ccCacheClusterStatus = a});

-- | The name of the Availability Zone in which the cache cluster is located
-- or \"Multiple\" if the cache nodes are located in different Availability
-- Zones.
ccPreferredAvailabilityZone :: Lens' CacheCluster (Maybe Text)
ccPreferredAvailabilityZone = lens _ccPreferredAvailabilityZone (\ s a -> s{_ccPreferredAvailabilityZone = a});

-- | Undocumented member.
ccCacheParameterGroup :: Lens' CacheCluster (Maybe CacheParameterGroupStatus)
ccCacheParameterGroup = lens _ccCacheParameterGroup (\ s a -> s{_ccCacheParameterGroup = a});

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- /SnapshotRetentionLimit/ to 5, then a snapshot that was taken today will
-- be retained for 5 days before being deleted.
--
-- __Important__
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
ccSnapshotRetentionLimit :: Lens' CacheCluster (Maybe Int)
ccSnapshotRetentionLimit = lens _ccSnapshotRetentionLimit (\ s a -> s{_ccSnapshotRetentionLimit = a});

-- | The replication group to which this cache cluster belongs. If this field
-- is empty, the cache cluster is not associated with any replication
-- group.
ccReplicationGroupId :: Lens' CacheCluster (Maybe Text)
ccReplicationGroupId = lens _ccReplicationGroupId (\ s a -> s{_ccReplicationGroupId = a});

-- | Undocumented member.
ccPendingModifiedValues :: Lens' CacheCluster (Maybe PendingModifiedValues)
ccPendingModifiedValues = lens _ccPendingModifiedValues (\ s a -> s{_ccPendingModifiedValues = a});

-- | The number of cache nodes in the cache cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
ccNumCacheNodes :: Lens' CacheCluster (Maybe Int)
ccNumCacheNodes = lens _ccNumCacheNodes (\ s a -> s{_ccNumCacheNodes = a});

instance FromXML CacheCluster where
        parseXML x
          = CacheCluster' <$>
              (x .@? "CacheNodeType") <*> (x .@? "EngineVersion")
                <*>
                (x .@? "CacheNodes" .!@ mempty >>=
                   may (parseXMLList "CacheNode"))
                <*> (x .@? "CacheClusterCreateTime")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "NotificationConfiguration")
                <*> (x .@? "SnapshotWindow")
                <*> (x .@? "CacheClusterId")
                <*> (x .@? "ConfigurationEndpoint")
                <*> (x .@? "Engine")
                <*>
                (x .@? "CacheSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "CacheSecurityGroup"))
                <*> (x .@? "ClientDownloadLandingPage")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "CacheSubnetGroupName")
                <*> (x .@? "CacheClusterStatus")
                <*> (x .@? "PreferredAvailabilityZone")
                <*> (x .@? "CacheParameterGroup")
                <*> (x .@? "SnapshotRetentionLimit")
                <*> (x .@? "ReplicationGroupId")
                <*> (x .@? "PendingModifiedValues")
                <*> (x .@? "NumCacheNodes")

-- | Provides all of the details about a particular cache engine version.
--
-- /See:/ 'cacheEngineVersion' smart constructor.
data CacheEngineVersion = CacheEngineVersion'
    { _cevCacheEngineDescription        :: !(Maybe Text)
    , _cevCacheParameterGroupFamily     :: !(Maybe Text)
    , _cevEngineVersion                 :: !(Maybe Text)
    , _cevCacheEngineVersionDescription :: !(Maybe Text)
    , _cevEngine                        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheEngineVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cevCacheEngineDescription'
--
-- * 'cevCacheParameterGroupFamily'
--
-- * 'cevEngineVersion'
--
-- * 'cevCacheEngineVersionDescription'
--
-- * 'cevEngine'
cacheEngineVersion
    :: CacheEngineVersion
cacheEngineVersion =
    CacheEngineVersion'
    { _cevCacheEngineDescription = Nothing
    , _cevCacheParameterGroupFamily = Nothing
    , _cevEngineVersion = Nothing
    , _cevCacheEngineVersionDescription = Nothing
    , _cevEngine = Nothing
    }

-- | The description of the cache engine.
cevCacheEngineDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineDescription = lens _cevCacheEngineDescription (\ s a -> s{_cevCacheEngineDescription = a});

-- | The name of the cache parameter group family associated with this cache
-- engine.
cevCacheParameterGroupFamily :: Lens' CacheEngineVersion (Maybe Text)
cevCacheParameterGroupFamily = lens _cevCacheParameterGroupFamily (\ s a -> s{_cevCacheParameterGroupFamily = a});

-- | The version number of the cache engine.
cevEngineVersion :: Lens' CacheEngineVersion (Maybe Text)
cevEngineVersion = lens _cevEngineVersion (\ s a -> s{_cevEngineVersion = a});

-- | The description of the cache engine version.
cevCacheEngineVersionDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineVersionDescription = lens _cevCacheEngineVersionDescription (\ s a -> s{_cevCacheEngineVersionDescription = a});

-- | The name of the cache engine.
cevEngine :: Lens' CacheEngineVersion (Maybe Text)
cevEngine = lens _cevEngine (\ s a -> s{_cevEngine = a});

instance FromXML CacheEngineVersion where
        parseXML x
          = CacheEngineVersion' <$>
              (x .@? "CacheEngineDescription") <*>
                (x .@? "CacheParameterGroupFamily")
                <*> (x .@? "EngineVersion")
                <*> (x .@? "CacheEngineVersionDescription")
                <*> (x .@? "Engine")

-- | Represents an individual cache node within a cache cluster. Each cache
-- node runs its own instance of the cluster\'s protocol-compliant caching
-- software - either Memcached or Redis.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: 'cache.t2.micro', 'cache.t2.small',
--         'cache.t2.medium', 'cache.m3.medium', 'cache.m3.large',
--         'cache.m3.xlarge', 'cache.m3.2xlarge'
--     -   Previous generation: 'cache.t1.micro', 'cache.m1.small',
--         'cache.m1.medium', 'cache.m1.large', 'cache.m1.xlarge'
-- -   Compute optimized: 'cache.c1.xlarge'
-- -   Memory optimized
--     -   Current generation: 'cache.r3.large', 'cache.r3.xlarge',
--         'cache.r3.2xlarge', 'cache.r3.4xlarge', 'cache.r3.8xlarge'
--     -   Previous generation: 'cache.m2.xlarge', 'cache.m2.2xlarge',
--         'cache.m2.4xlarge'
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
--
-- /See:/ 'cacheNode' smart constructor.
data CacheNode = CacheNode'
    { _cnSourceCacheNodeId        :: !(Maybe Text)
    , _cnParameterGroupStatus     :: !(Maybe Text)
    , _cnCacheNodeCreateTime      :: !(Maybe ISO8601)
    , _cnCustomerAvailabilityZone :: !(Maybe Text)
    , _cnCacheNodeId              :: !(Maybe Text)
    , _cnCacheNodeStatus          :: !(Maybe Text)
    , _cnEndpoint                 :: !(Maybe Endpoint)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnSourceCacheNodeId'
--
-- * 'cnParameterGroupStatus'
--
-- * 'cnCacheNodeCreateTime'
--
-- * 'cnCustomerAvailabilityZone'
--
-- * 'cnCacheNodeId'
--
-- * 'cnCacheNodeStatus'
--
-- * 'cnEndpoint'
cacheNode
    :: CacheNode
cacheNode =
    CacheNode'
    { _cnSourceCacheNodeId = Nothing
    , _cnParameterGroupStatus = Nothing
    , _cnCacheNodeCreateTime = Nothing
    , _cnCustomerAvailabilityZone = Nothing
    , _cnCacheNodeId = Nothing
    , _cnCacheNodeStatus = Nothing
    , _cnEndpoint = Nothing
    }

-- | The ID of the primary node to which this read replica node is
-- synchronized. If this field is empty, then this node is not associated
-- with a primary cache cluster.
cnSourceCacheNodeId :: Lens' CacheNode (Maybe Text)
cnSourceCacheNodeId = lens _cnSourceCacheNodeId (\ s a -> s{_cnSourceCacheNodeId = a});

-- | The status of the parameter group applied to this cache node.
cnParameterGroupStatus :: Lens' CacheNode (Maybe Text)
cnParameterGroupStatus = lens _cnParameterGroupStatus (\ s a -> s{_cnParameterGroupStatus = a});

-- | The date and time when the cache node was created.
cnCacheNodeCreateTime :: Lens' CacheNode (Maybe UTCTime)
cnCacheNodeCreateTime = lens _cnCacheNodeCreateTime (\ s a -> s{_cnCacheNodeCreateTime = a}) . mapping _Time;

-- | The Availability Zone where this node was created and now resides.
cnCustomerAvailabilityZone :: Lens' CacheNode (Maybe Text)
cnCustomerAvailabilityZone = lens _cnCustomerAvailabilityZone (\ s a -> s{_cnCustomerAvailabilityZone = a});

-- | The cache node identifier. A node ID is a numeric identifier (0001,
-- 0002, etc.). The combination of cluster ID and node ID uniquely
-- identifies every cache node used in a customer\'s AWS account.
cnCacheNodeId :: Lens' CacheNode (Maybe Text)
cnCacheNodeId = lens _cnCacheNodeId (\ s a -> s{_cnCacheNodeId = a});

-- | The current state of this cache node.
cnCacheNodeStatus :: Lens' CacheNode (Maybe Text)
cnCacheNodeStatus = lens _cnCacheNodeStatus (\ s a -> s{_cnCacheNodeStatus = a});

-- | The hostname for connecting to this cache node.
cnEndpoint :: Lens' CacheNode (Maybe Endpoint)
cnEndpoint = lens _cnEndpoint (\ s a -> s{_cnEndpoint = a});

instance FromXML CacheNode where
        parseXML x
          = CacheNode' <$>
              (x .@? "SourceCacheNodeId") <*>
                (x .@? "ParameterGroupStatus")
                <*> (x .@? "CacheNodeCreateTime")
                <*> (x .@? "CustomerAvailabilityZone")
                <*> (x .@? "CacheNodeId")
                <*> (x .@? "CacheNodeStatus")
                <*> (x .@? "Endpoint")

-- | A parameter that has a different value for each cache node type it is
-- applied to. For example, in a Redis cache cluster, a /cache.m1.large/
-- cache node type would have a larger /maxmemory/ value than a
-- /cache.m1.small/ type.
--
-- /See:/ 'cacheNodeTypeSpecificParameter' smart constructor.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter'
    { _cntspCacheNodeTypeSpecificValues :: !(Maybe [CacheNodeTypeSpecificValue])
    , _cntspMinimumEngineVersion        :: !(Maybe Text)
    , _cntspSource                      :: !(Maybe Text)
    , _cntspIsModifiable                :: !(Maybe Bool)
    , _cntspAllowedValues               :: !(Maybe Text)
    , _cntspDataType                    :: !(Maybe Text)
    , _cntspParameterName               :: !(Maybe Text)
    , _cntspDescription                 :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheNodeTypeSpecificParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cntspCacheNodeTypeSpecificValues'
--
-- * 'cntspMinimumEngineVersion'
--
-- * 'cntspSource'
--
-- * 'cntspIsModifiable'
--
-- * 'cntspAllowedValues'
--
-- * 'cntspDataType'
--
-- * 'cntspParameterName'
--
-- * 'cntspDescription'
cacheNodeTypeSpecificParameter
    :: CacheNodeTypeSpecificParameter
cacheNodeTypeSpecificParameter =
    CacheNodeTypeSpecificParameter'
    { _cntspCacheNodeTypeSpecificValues = Nothing
    , _cntspMinimumEngineVersion = Nothing
    , _cntspSource = Nothing
    , _cntspIsModifiable = Nothing
    , _cntspAllowedValues = Nothing
    , _cntspDataType = Nothing
    , _cntspParameterName = Nothing
    , _cntspDescription = Nothing
    }

-- | A list of cache node types and their corresponding values for this
-- parameter.
cntspCacheNodeTypeSpecificValues :: Lens' CacheNodeTypeSpecificParameter [CacheNodeTypeSpecificValue]
cntspCacheNodeTypeSpecificValues = lens _cntspCacheNodeTypeSpecificValues (\ s a -> s{_cntspCacheNodeTypeSpecificValues = a}) . _Default . _Coerce;

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspMinimumEngineVersion = lens _cntspMinimumEngineVersion (\ s a -> s{_cntspMinimumEngineVersion = a});

-- | The source of the parameter value.
cntspSource :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspSource = lens _cntspSource (\ s a -> s{_cntspSource = a});

-- | Indicates whether ('true') or not ('false') the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
cntspIsModifiable :: Lens' CacheNodeTypeSpecificParameter (Maybe Bool)
cntspIsModifiable = lens _cntspIsModifiable (\ s a -> s{_cntspIsModifiable = a});

-- | The valid range of values for the parameter.
cntspAllowedValues :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspAllowedValues = lens _cntspAllowedValues (\ s a -> s{_cntspAllowedValues = a});

-- | The valid data type for the parameter.
cntspDataType :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDataType = lens _cntspDataType (\ s a -> s{_cntspDataType = a});

-- | The name of the parameter.
cntspParameterName :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspParameterName = lens _cntspParameterName (\ s a -> s{_cntspParameterName = a});

-- | A description of the parameter.
cntspDescription :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDescription = lens _cntspDescription (\ s a -> s{_cntspDescription = a});

instance FromXML CacheNodeTypeSpecificParameter where
        parseXML x
          = CacheNodeTypeSpecificParameter' <$>
              (x .@? "CacheNodeTypeSpecificValues" .!@ mempty >>=
                 may (parseXMLList "CacheNodeTypeSpecificValue"))
                <*> (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "DataType")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")

-- | A value that applies only to a certain cache node type.
--
-- /See:/ 'cacheNodeTypeSpecificValue' smart constructor.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue'
    { _cntsvCacheNodeType :: !(Maybe Text)
    , _cntsvValue         :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheNodeTypeSpecificValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cntsvCacheNodeType'
--
-- * 'cntsvValue'
cacheNodeTypeSpecificValue
    :: CacheNodeTypeSpecificValue
cacheNodeTypeSpecificValue =
    CacheNodeTypeSpecificValue'
    { _cntsvCacheNodeType = Nothing
    , _cntsvValue = Nothing
    }

-- | The cache node type for which this value applies.
cntsvCacheNodeType :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvCacheNodeType = lens _cntsvCacheNodeType (\ s a -> s{_cntsvCacheNodeType = a});

-- | The value for the cache node type.
cntsvValue :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvValue = lens _cntsvValue (\ s a -> s{_cntsvValue = a});

instance FromXML CacheNodeTypeSpecificValue where
        parseXML x
          = CacheNodeTypeSpecificValue' <$>
              (x .@? "CacheNodeType") <*> (x .@? "Value")

-- | Represents the output of a /CreateCacheParameterGroup/ action.
--
-- /See:/ 'cacheParameterGroup' smart constructor.
data CacheParameterGroup = CacheParameterGroup'
    { _cpgCacheParameterGroupFamily :: !(Maybe Text)
    , _cpgCacheParameterGroupName   :: !(Maybe Text)
    , _cpgDescription               :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgCacheParameterGroupFamily'
--
-- * 'cpgCacheParameterGroupName'
--
-- * 'cpgDescription'
cacheParameterGroup
    :: CacheParameterGroup
cacheParameterGroup =
    CacheParameterGroup'
    { _cpgCacheParameterGroupFamily = Nothing
    , _cpgCacheParameterGroupName = Nothing
    , _cpgDescription = Nothing
    }

-- | The name of the cache parameter group family that this cache parameter
-- group is compatible with.
cpgCacheParameterGroupFamily :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupFamily = lens _cpgCacheParameterGroupFamily (\ s a -> s{_cpgCacheParameterGroupFamily = a});

-- | The name of the cache parameter group.
cpgCacheParameterGroupName :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupName = lens _cpgCacheParameterGroupName (\ s a -> s{_cpgCacheParameterGroupName = a});

-- | The description for this cache parameter group.
cpgDescription :: Lens' CacheParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\ s a -> s{_cpgDescription = a});

instance FromXML CacheParameterGroup where
        parseXML x
          = CacheParameterGroup' <$>
              (x .@? "CacheParameterGroupFamily") <*>
                (x .@? "CacheParameterGroupName")
                <*> (x .@? "Description")

-- | Represents the output of one of the following actions:
--
-- -   /ModifyCacheParameterGroup/
-- -   /ResetCacheParameterGroup/
--
-- /See:/ 'cacheParameterGroupNameMessage' smart constructor.
newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage'
    { _cpgnmCacheParameterGroupName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgnmCacheParameterGroupName'
cacheParameterGroupNameMessage
    :: CacheParameterGroupNameMessage
cacheParameterGroupNameMessage =
    CacheParameterGroupNameMessage'
    { _cpgnmCacheParameterGroupName = Nothing
    }

-- | The name of the cache parameter group.
cpgnmCacheParameterGroupName :: Lens' CacheParameterGroupNameMessage (Maybe Text)
cpgnmCacheParameterGroupName = lens _cpgnmCacheParameterGroupName (\ s a -> s{_cpgnmCacheParameterGroupName = a});

instance FromXML CacheParameterGroupNameMessage where
        parseXML x
          = CacheParameterGroupNameMessage' <$>
              (x .@? "CacheParameterGroupName")

-- | The status of the cache parameter group.
--
-- /See:/ 'cacheParameterGroupStatus' smart constructor.
data CacheParameterGroupStatus = CacheParameterGroupStatus'
    { _cpgsCacheParameterGroupName :: !(Maybe Text)
    , _cpgsCacheNodeIdsToReboot    :: !(Maybe [Text])
    , _cpgsParameterApplyStatus    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgsCacheParameterGroupName'
--
-- * 'cpgsCacheNodeIdsToReboot'
--
-- * 'cpgsParameterApplyStatus'
cacheParameterGroupStatus
    :: CacheParameterGroupStatus
cacheParameterGroupStatus =
    CacheParameterGroupStatus'
    { _cpgsCacheParameterGroupName = Nothing
    , _cpgsCacheNodeIdsToReboot = Nothing
    , _cpgsParameterApplyStatus = Nothing
    }

-- | The name of the cache parameter group.
cpgsCacheParameterGroupName :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsCacheParameterGroupName = lens _cpgsCacheParameterGroupName (\ s a -> s{_cpgsCacheParameterGroupName = a});

-- | A list of the cache node IDs which need to be rebooted for parameter
-- changes to be applied. A node ID is a numeric identifier (0001, 0002,
-- etc.).
cpgsCacheNodeIdsToReboot :: Lens' CacheParameterGroupStatus [Text]
cpgsCacheNodeIdsToReboot = lens _cpgsCacheNodeIdsToReboot (\ s a -> s{_cpgsCacheNodeIdsToReboot = a}) . _Default . _Coerce;

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus = lens _cpgsParameterApplyStatus (\ s a -> s{_cpgsParameterApplyStatus = a});

instance FromXML CacheParameterGroupStatus where
        parseXML x
          = CacheParameterGroupStatus' <$>
              (x .@? "CacheParameterGroupName") <*>
                (x .@? "CacheNodeIdsToReboot" .!@ mempty >>=
                   may (parseXMLList "CacheNodeId"))
                <*> (x .@? "ParameterApplyStatus")

-- | Represents the output of one of the following actions:
--
-- -   /AuthorizeCacheSecurityGroupIngress/
-- -   /CreateCacheSecurityGroup/
-- -   /RevokeCacheSecurityGroupIngress/
--
-- /See:/ 'cacheSecurityGroup' smart constructor.
data CacheSecurityGroup = CacheSecurityGroup'
    { _csgCacheSecurityGroupName :: !(Maybe Text)
    , _csgOwnerId                :: !(Maybe Text)
    , _csgEC2SecurityGroups      :: !(Maybe [EC2SecurityGroup])
    , _csgDescription            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgCacheSecurityGroupName'
--
-- * 'csgOwnerId'
--
-- * 'csgEC2SecurityGroups'
--
-- * 'csgDescription'
cacheSecurityGroup
    :: CacheSecurityGroup
cacheSecurityGroup =
    CacheSecurityGroup'
    { _csgCacheSecurityGroupName = Nothing
    , _csgOwnerId = Nothing
    , _csgEC2SecurityGroups = Nothing
    , _csgDescription = Nothing
    }

-- | The name of the cache security group.
csgCacheSecurityGroupName :: Lens' CacheSecurityGroup (Maybe Text)
csgCacheSecurityGroupName = lens _csgCacheSecurityGroupName (\ s a -> s{_csgCacheSecurityGroupName = a});

-- | The AWS account ID of the cache security group owner.
csgOwnerId :: Lens' CacheSecurityGroup (Maybe Text)
csgOwnerId = lens _csgOwnerId (\ s a -> s{_csgOwnerId = a});

-- | A list of Amazon EC2 security groups that are associated with this cache
-- security group.
csgEC2SecurityGroups :: Lens' CacheSecurityGroup [EC2SecurityGroup]
csgEC2SecurityGroups = lens _csgEC2SecurityGroups (\ s a -> s{_csgEC2SecurityGroups = a}) . _Default . _Coerce;

-- | The description of the cache security group.
csgDescription :: Lens' CacheSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\ s a -> s{_csgDescription = a});

instance FromXML CacheSecurityGroup where
        parseXML x
          = CacheSecurityGroup' <$>
              (x .@? "CacheSecurityGroupName") <*>
                (x .@? "OwnerId")
                <*>
                (x .@? "EC2SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "EC2SecurityGroup"))
                <*> (x .@? "Description")

-- | Represents a cache cluster\'s status within a particular cache security
-- group.
--
-- /See:/ 'cacheSecurityGroupMembership' smart constructor.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership'
    { _csgmStatus                 :: !(Maybe Text)
    , _csgmCacheSecurityGroupName :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgmStatus'
--
-- * 'csgmCacheSecurityGroupName'
cacheSecurityGroupMembership
    :: CacheSecurityGroupMembership
cacheSecurityGroupMembership =
    CacheSecurityGroupMembership'
    { _csgmStatus = Nothing
    , _csgmCacheSecurityGroupName = Nothing
    }

-- | The membership status in the cache security group. The status changes
-- when a cache security group is modified, or when the cache security
-- groups assigned to a cache cluster are modified.
csgmStatus :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\ s a -> s{_csgmStatus = a});

-- | The name of the cache security group.
csgmCacheSecurityGroupName :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmCacheSecurityGroupName = lens _csgmCacheSecurityGroupName (\ s a -> s{_csgmCacheSecurityGroupName = a});

instance FromXML CacheSecurityGroupMembership where
        parseXML x
          = CacheSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "CacheSecurityGroupName")

-- | Represents the output of one of the following actions:
--
-- -   /CreateCacheSubnetGroup/
-- -   /ModifyCacheSubnetGroup/
--
-- /See:/ 'cacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'
    { _csgVPCId                       :: !(Maybe Text)
    , _csgSubnets                     :: !(Maybe [Subnet])
    , _csgCacheSubnetGroupName        :: !(Maybe Text)
    , _csgCacheSubnetGroupDescription :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CacheSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgVPCId'
--
-- * 'csgSubnets'
--
-- * 'csgCacheSubnetGroupName'
--
-- * 'csgCacheSubnetGroupDescription'
cacheSubnetGroup
    :: CacheSubnetGroup
cacheSubnetGroup =
    CacheSubnetGroup'
    { _csgVPCId = Nothing
    , _csgSubnets = Nothing
    , _csgCacheSubnetGroupName = Nothing
    , _csgCacheSubnetGroupDescription = Nothing
    }

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group.
csgVPCId :: Lens' CacheSubnetGroup (Maybe Text)
csgVPCId = lens _csgVPCId (\ s a -> s{_csgVPCId = a});

-- | A list of subnets associated with the cache subnet group.
csgSubnets :: Lens' CacheSubnetGroup [Subnet]
csgSubnets = lens _csgSubnets (\ s a -> s{_csgSubnets = a}) . _Default . _Coerce;

-- | The name of the cache subnet group.
csgCacheSubnetGroupName :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupName = lens _csgCacheSubnetGroupName (\ s a -> s{_csgCacheSubnetGroupName = a});

-- | The description of the cache subnet group.
csgCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupDescription = lens _csgCacheSubnetGroupDescription (\ s a -> s{_csgCacheSubnetGroupDescription = a});

instance FromXML CacheSubnetGroup where
        parseXML x
          = CacheSubnetGroup' <$>
              (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "CacheSubnetGroupName")
                <*> (x .@? "CacheSubnetGroupDescription")

-- | Provides ownership and status information for an Amazon EC2 security
-- group.
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
    { _esgStatus                  :: !(Maybe Text)
    , _esgEC2SecurityGroupOwnerId :: !(Maybe Text)
    , _esgEC2SecurityGroupName    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EC2SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esgStatus'
--
-- * 'esgEC2SecurityGroupOwnerId'
--
-- * 'esgEC2SecurityGroupName'
ec2SecurityGroup
    :: EC2SecurityGroup
ec2SecurityGroup =
    EC2SecurityGroup'
    { _esgStatus = Nothing
    , _esgEC2SecurityGroupOwnerId = Nothing
    , _esgEC2SecurityGroupName = Nothing
    }

-- | The status of the Amazon EC2 security group.
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\ s a -> s{_esgStatus = a});

-- | The AWS account ID of the Amazon EC2 security group owner.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\ s a -> s{_esgEC2SecurityGroupOwnerId = a});

-- | The name of the Amazon EC2 security group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\ s a -> s{_esgEC2SecurityGroupName = a});

instance FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' <$>
              (x .@? "Status") <*>
                (x .@? "EC2SecurityGroupOwnerId")
                <*> (x .@? "EC2SecurityGroupName")

-- | Represents the information required for client programs to connect to a
-- cache node.
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
    { _eAddress :: !(Maybe Text)
    , _ePort    :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAddress'
--
-- * 'ePort'
endpoint
    :: Endpoint
endpoint =
    Endpoint'
    { _eAddress = Nothing
    , _ePort = Nothing
    }

-- | The DNS hostname of the cache node.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a});

-- | The port number that the cache engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a});

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$> (x .@? "Address") <*> (x .@? "Port")

-- | Represents the output of a /DescribeEngineDefaultParameters/ action.
--
-- /See:/ 'engineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
    { _edCacheParameterGroupFamily       :: !(Maybe Text)
    , _edCacheNodeTypeSpecificParameters :: !(Maybe [CacheNodeTypeSpecificParameter])
    , _edParameters                      :: !(Maybe [Parameter])
    , _edMarker                          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edCacheParameterGroupFamily'
--
-- * 'edCacheNodeTypeSpecificParameters'
--
-- * 'edParameters'
--
-- * 'edMarker'
engineDefaults
    :: EngineDefaults
engineDefaults =
    EngineDefaults'
    { _edCacheParameterGroupFamily = Nothing
    , _edCacheNodeTypeSpecificParameters = Nothing
    , _edParameters = Nothing
    , _edMarker = Nothing
    }

-- | Specifies the name of the cache parameter group family to which the
-- engine default parameters apply.
edCacheParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edCacheParameterGroupFamily = lens _edCacheParameterGroupFamily (\ s a -> s{_edCacheParameterGroupFamily = a});

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters :: Lens' EngineDefaults [CacheNodeTypeSpecificParameter]
edCacheNodeTypeSpecificParameters = lens _edCacheNodeTypeSpecificParameters (\ s a -> s{_edCacheNodeTypeSpecificParameters = a}) . _Default . _Coerce;

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\ s a -> s{_edParameters = a}) . _Default . _Coerce;

-- | Provides an identifier to allow retrieval of paginated results.
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\ s a -> s{_edMarker = a});

instance FromXML EngineDefaults where
        parseXML x
          = EngineDefaults' <$>
              (x .@? "CacheParameterGroupFamily") <*>
                (x .@? "CacheNodeTypeSpecificParameters" .!@ mempty
                   >>=
                   may (parseXMLList "CacheNodeTypeSpecificParameter"))
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "Parameter"))
                <*> (x .@? "Marker")

-- | Represents a single occurrence of something interesting within the
-- system. Some examples of events are creating a cache cluster, adding or
-- removing a cache node, or rebooting a node.
--
-- /See:/ 'event' smart constructor.
data Event = Event'
    { _eSourceType       :: !(Maybe SourceType)
    , _eSourceIdentifier :: !(Maybe Text)
    , _eDate             :: !(Maybe ISO8601)
    , _eMessage          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType'
--
-- * 'eSourceIdentifier'
--
-- * 'eDate'
--
-- * 'eMessage'
event
    :: Event
event =
    Event'
    { _eSourceType = Nothing
    , _eSourceIdentifier = Nothing
    , _eDate = Nothing
    , _eMessage = Nothing
    }

-- | Specifies the origin of this event - a cache cluster, a parameter group,
-- a security group, etc.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a});

-- | The identifier for the source of the event. For example, if the event
-- occurred at the cache cluster level, the identifier would be the name of
-- the cache cluster.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\ s a -> s{_eSourceIdentifier = a});

-- | The date and time when the event occurred.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time;

-- | The text of the event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a});

instance FromXML Event where
        parseXML x
          = Event' <$>
              (x .@? "SourceType") <*> (x .@? "SourceIdentifier")
                <*> (x .@? "Date")
                <*> (x .@? "Message")

-- | Represents a collection of cache nodes in a replication group.
--
-- /See:/ 'nodeGroup' smart constructor.
data NodeGroup = NodeGroup'
    { _ngStatus           :: !(Maybe Text)
    , _ngPrimaryEndpoint  :: !(Maybe Endpoint)
    , _ngNodeGroupMembers :: !(Maybe [NodeGroupMember])
    , _ngNodeGroupId      :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NodeGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngStatus'
--
-- * 'ngPrimaryEndpoint'
--
-- * 'ngNodeGroupMembers'
--
-- * 'ngNodeGroupId'
nodeGroup
    :: NodeGroup
nodeGroup =
    NodeGroup'
    { _ngStatus = Nothing
    , _ngPrimaryEndpoint = Nothing
    , _ngNodeGroupMembers = Nothing
    , _ngNodeGroupId = Nothing
    }

-- | The current state of this replication group - /creating/, /available/,
-- etc.
ngStatus :: Lens' NodeGroup (Maybe Text)
ngStatus = lens _ngStatus (\ s a -> s{_ngStatus = a});

-- | Undocumented member.
ngPrimaryEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngPrimaryEndpoint = lens _ngPrimaryEndpoint (\ s a -> s{_ngPrimaryEndpoint = a});

-- | A list containing information about individual nodes within the node
-- group.
ngNodeGroupMembers :: Lens' NodeGroup [NodeGroupMember]
ngNodeGroupMembers = lens _ngNodeGroupMembers (\ s a -> s{_ngNodeGroupMembers = a}) . _Default . _Coerce;

-- | The identifier for the node group. A replication group contains only one
-- node group; therefore, the node group ID is 0001.
ngNodeGroupId :: Lens' NodeGroup (Maybe Text)
ngNodeGroupId = lens _ngNodeGroupId (\ s a -> s{_ngNodeGroupId = a});

instance FromXML NodeGroup where
        parseXML x
          = NodeGroup' <$>
              (x .@? "Status") <*> (x .@? "PrimaryEndpoint") <*>
                (x .@? "NodeGroupMembers" .!@ mempty >>=
                   may (parseXMLList "NodeGroupMember"))
                <*> (x .@? "NodeGroupId")

-- | Represents a single node within a node group.
--
-- /See:/ 'nodeGroupMember' smart constructor.
data NodeGroupMember = NodeGroupMember'
    { _ngmCacheClusterId            :: !(Maybe Text)
    , _ngmCacheNodeId               :: !(Maybe Text)
    , _ngmPreferredAvailabilityZone :: !(Maybe Text)
    , _ngmCurrentRole               :: !(Maybe Text)
    , _ngmReadEndpoint              :: !(Maybe Endpoint)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NodeGroupMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngmCacheClusterId'
--
-- * 'ngmCacheNodeId'
--
-- * 'ngmPreferredAvailabilityZone'
--
-- * 'ngmCurrentRole'
--
-- * 'ngmReadEndpoint'
nodeGroupMember
    :: NodeGroupMember
nodeGroupMember =
    NodeGroupMember'
    { _ngmCacheClusterId = Nothing
    , _ngmCacheNodeId = Nothing
    , _ngmPreferredAvailabilityZone = Nothing
    , _ngmCurrentRole = Nothing
    , _ngmReadEndpoint = Nothing
    }

-- | The ID of the cache cluster to which the node belongs.
ngmCacheClusterId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheClusterId = lens _ngmCacheClusterId (\ s a -> s{_ngmCacheClusterId = a});

-- | The ID of the node within its cache cluster. A node ID is a numeric
-- identifier (0001, 0002, etc.).
ngmCacheNodeId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheNodeId = lens _ngmCacheNodeId (\ s a -> s{_ngmCacheNodeId = a});

-- | The name of the Availability Zone in which the node is located.
ngmPreferredAvailabilityZone :: Lens' NodeGroupMember (Maybe Text)
ngmPreferredAvailabilityZone = lens _ngmPreferredAvailabilityZone (\ s a -> s{_ngmPreferredAvailabilityZone = a});

-- | The role that is currently assigned to the node - /primary/ or
-- /replica/.
ngmCurrentRole :: Lens' NodeGroupMember (Maybe Text)
ngmCurrentRole = lens _ngmCurrentRole (\ s a -> s{_ngmCurrentRole = a});

-- | Undocumented member.
ngmReadEndpoint :: Lens' NodeGroupMember (Maybe Endpoint)
ngmReadEndpoint = lens _ngmReadEndpoint (\ s a -> s{_ngmReadEndpoint = a});

instance FromXML NodeGroupMember where
        parseXML x
          = NodeGroupMember' <$>
              (x .@? "CacheClusterId") <*> (x .@? "CacheNodeId")
                <*> (x .@? "PreferredAvailabilityZone")
                <*> (x .@? "CurrentRole")
                <*> (x .@? "ReadEndpoint")

-- | Represents an individual cache node in a snapshot of a cache cluster.
--
-- /See:/ 'nodeSnapshot' smart constructor.
data NodeSnapshot = NodeSnapshot'
    { _nsCacheNodeCreateTime :: !(Maybe ISO8601)
    , _nsCacheNodeId         :: !(Maybe Text)
    , _nsSnapshotCreateTime  :: !(Maybe ISO8601)
    , _nsCacheSize           :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NodeSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nsCacheNodeCreateTime'
--
-- * 'nsCacheNodeId'
--
-- * 'nsSnapshotCreateTime'
--
-- * 'nsCacheSize'
nodeSnapshot
    :: NodeSnapshot
nodeSnapshot =
    NodeSnapshot'
    { _nsCacheNodeCreateTime = Nothing
    , _nsCacheNodeId = Nothing
    , _nsSnapshotCreateTime = Nothing
    , _nsCacheSize = Nothing
    }

-- | The date and time when the cache node was created in the source cache
-- cluster.
nsCacheNodeCreateTime :: Lens' NodeSnapshot (Maybe UTCTime)
nsCacheNodeCreateTime = lens _nsCacheNodeCreateTime (\ s a -> s{_nsCacheNodeCreateTime = a}) . mapping _Time;

-- | The cache node identifier for the node in the source cache cluster.
nsCacheNodeId :: Lens' NodeSnapshot (Maybe Text)
nsCacheNodeId = lens _nsCacheNodeId (\ s a -> s{_nsCacheNodeId = a});

-- | The date and time when the source node\'s metadata and cache data set
-- was obtained for the snapshot.
nsSnapshotCreateTime :: Lens' NodeSnapshot (Maybe UTCTime)
nsSnapshotCreateTime = lens _nsSnapshotCreateTime (\ s a -> s{_nsSnapshotCreateTime = a}) . mapping _Time;

-- | The size of the cache on the source cache node.
nsCacheSize :: Lens' NodeSnapshot (Maybe Text)
nsCacheSize = lens _nsCacheSize (\ s a -> s{_nsCacheSize = a});

instance FromXML NodeSnapshot where
        parseXML x
          = NodeSnapshot' <$>
              (x .@? "CacheNodeCreateTime") <*>
                (x .@? "CacheNodeId")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "CacheSize")

-- | Describes a notification topic and its status. Notification topics are
-- used for publishing ElastiCache events to subscribers using Amazon
-- Simple Notification Service (SNS).
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
    { _ncTopicStatus :: !(Maybe Text)
    , _ncTopicARN    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncTopicStatus'
--
-- * 'ncTopicARN'
notificationConfiguration
    :: NotificationConfiguration
notificationConfiguration =
    NotificationConfiguration'
    { _ncTopicStatus = Nothing
    , _ncTopicARN = Nothing
    }

-- | The current state of the topic.
ncTopicStatus :: Lens' NotificationConfiguration (Maybe Text)
ncTopicStatus = lens _ncTopicStatus (\ s a -> s{_ncTopicStatus = a});

-- | The Amazon Resource Name (ARN) that identifies the topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\ s a -> s{_ncTopicARN = a});

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (x .@? "TopicStatus") <*> (x .@? "TopicArn")

-- | Describes an individual setting that controls some aspect of ElastiCache
-- behavior.
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
    { _pParameterValue       :: !(Maybe Text)
    , _pMinimumEngineVersion :: !(Maybe Text)
    , _pSource               :: !(Maybe Text)
    , _pIsModifiable         :: !(Maybe Bool)
    , _pAllowedValues        :: !(Maybe Text)
    , _pDataType             :: !(Maybe Text)
    , _pParameterName        :: !(Maybe Text)
    , _pDescription          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameterValue'
--
-- * 'pMinimumEngineVersion'
--
-- * 'pSource'
--
-- * 'pIsModifiable'
--
-- * 'pAllowedValues'
--
-- * 'pDataType'
--
-- * 'pParameterName'
--
-- * 'pDescription'
parameter
    :: Parameter
parameter =
    Parameter'
    { _pParameterValue = Nothing
    , _pMinimumEngineVersion = Nothing
    , _pSource = Nothing
    , _pIsModifiable = Nothing
    , _pAllowedValues = Nothing
    , _pDataType = Nothing
    , _pParameterName = Nothing
    , _pDescription = Nothing
    }

-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a});

-- | The earliest cache engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\ s a -> s{_pMinimumEngineVersion = a});

-- | The source of the parameter.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\ s a -> s{_pSource = a});

-- | Indicates whether ('true') or not ('false') the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\ s a -> s{_pIsModifiable = a});

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\ s a -> s{_pAllowedValues = a});

-- | The valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\ s a -> s{_pDataType = a});

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\ s a -> s{_pParameterName = a});

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a});

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ParameterValue") <*>
                (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "DataType")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")

-- | Describes a name-value pair that is used to update the value of a
-- parameter.
--
-- /See:/ 'parameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
    { _pnvParameterValue :: !(Maybe Text)
    , _pnvParameterName  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ParameterNameValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnvParameterValue'
--
-- * 'pnvParameterName'
parameterNameValue
    :: ParameterNameValue
parameterNameValue =
    ParameterNameValue'
    { _pnvParameterValue = Nothing
    , _pnvParameterName = Nothing
    }

-- | The value of the parameter.
pnvParameterValue :: Lens' ParameterNameValue (Maybe Text)
pnvParameterValue = lens _pnvParameterValue (\ s a -> s{_pnvParameterValue = a});

-- | The name of the parameter.
pnvParameterName :: Lens' ParameterNameValue (Maybe Text)
pnvParameterName = lens _pnvParameterName (\ s a -> s{_pnvParameterName = a});

instance ToQuery ParameterNameValue where
        toQuery ParameterNameValue'{..}
          = mconcat
              ["ParameterValue" =: _pnvParameterValue,
               "ParameterName" =: _pnvParameterName]

-- | A group of settings that will be applied to the cache cluster in the
-- future, or that are currently being applied.
--
-- /See:/ 'pendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
    { _pmvEngineVersion        :: !(Maybe Text)
    , _pmvCacheNodeIdsToRemove :: !(Maybe [Text])
    , _pmvNumCacheNodes        :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvEngineVersion'
--
-- * 'pmvCacheNodeIdsToRemove'
--
-- * 'pmvNumCacheNodes'
pendingModifiedValues
    :: PendingModifiedValues
pendingModifiedValues =
    PendingModifiedValues'
    { _pmvEngineVersion = Nothing
    , _pmvCacheNodeIdsToRemove = Nothing
    , _pmvNumCacheNodes = Nothing
    }

-- | The new cache engine version that the cache cluster will run.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\ s a -> s{_pmvEngineVersion = a});

-- | A list of cache node IDs that are being removed (or will be removed)
-- from the cache cluster. A node ID is a numeric identifier (0001, 0002,
-- etc.).
pmvCacheNodeIdsToRemove :: Lens' PendingModifiedValues [Text]
pmvCacheNodeIdsToRemove = lens _pmvCacheNodeIdsToRemove (\ s a -> s{_pmvCacheNodeIdsToRemove = a}) . _Default . _Coerce;

-- | The new number of cache nodes for the cache cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
pmvNumCacheNodes :: Lens' PendingModifiedValues (Maybe Int)
pmvNumCacheNodes = lens _pmvNumCacheNodes (\ s a -> s{_pmvNumCacheNodes = a});

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "CacheNodeIdsToRemove" .!@ mempty >>=
                   may (parseXMLList "CacheNodeId"))
                <*> (x .@? "NumCacheNodes")

-- | Contains the specific price and frequency of a recurring charges for a
-- reserved cache node, or for a reserved cache node offering.
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
    { _rcRecurringChargeFrequency :: !(Maybe Text)
    , _rcRecurringChargeAmount    :: !(Maybe Double)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecurringChargeFrequency'
--
-- * 'rcRecurringChargeAmount'
recurringCharge
    :: RecurringCharge
recurringCharge =
    RecurringCharge'
    { _rcRecurringChargeFrequency = Nothing
    , _rcRecurringChargeAmount = Nothing
    }

-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a});

-- | The monetary amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a});

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "RecurringChargeFrequency") <*>
                (x .@? "RecurringChargeAmount")

-- | Contains all of the attributes of a specific replication group.
--
-- /See:/ 'replicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
    { _rgNodeGroups            :: !(Maybe [NodeGroup])
    , _rgStatus                :: !(Maybe Text)
    , _rgSnapshottingClusterId :: !(Maybe Text)
    , _rgMemberClusters        :: !(Maybe [Text])
    , _rgReplicationGroupId    :: !(Maybe Text)
    , _rgPendingModifiedValues :: !(Maybe ReplicationGroupPendingModifiedValues)
    , _rgDescription           :: !(Maybe Text)
    , _rgAutomaticFailover     :: !(Maybe AutomaticFailoverStatus)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgNodeGroups'
--
-- * 'rgStatus'
--
-- * 'rgSnapshottingClusterId'
--
-- * 'rgMemberClusters'
--
-- * 'rgReplicationGroupId'
--
-- * 'rgPendingModifiedValues'
--
-- * 'rgDescription'
--
-- * 'rgAutomaticFailover'
replicationGroup
    :: ReplicationGroup
replicationGroup =
    ReplicationGroup'
    { _rgNodeGroups = Nothing
    , _rgStatus = Nothing
    , _rgSnapshottingClusterId = Nothing
    , _rgMemberClusters = Nothing
    , _rgReplicationGroupId = Nothing
    , _rgPendingModifiedValues = Nothing
    , _rgDescription = Nothing
    , _rgAutomaticFailover = Nothing
    }

-- | A single element list with information about the nodes in the
-- replication group.
rgNodeGroups :: Lens' ReplicationGroup [NodeGroup]
rgNodeGroups = lens _rgNodeGroups (\ s a -> s{_rgNodeGroups = a}) . _Default . _Coerce;

-- | The current state of this replication group - /creating/, /available/,
-- etc.
rgStatus :: Lens' ReplicationGroup (Maybe Text)
rgStatus = lens _rgStatus (\ s a -> s{_rgStatus = a});

-- | The cache cluster ID that is used as the daily snapshot source for the
-- replication group.
rgSnapshottingClusterId :: Lens' ReplicationGroup (Maybe Text)
rgSnapshottingClusterId = lens _rgSnapshottingClusterId (\ s a -> s{_rgSnapshottingClusterId = a});

-- | The names of all the cache clusters that are part of this replication
-- group.
rgMemberClusters :: Lens' ReplicationGroup [Text]
rgMemberClusters = lens _rgMemberClusters (\ s a -> s{_rgMemberClusters = a}) . _Default . _Coerce;

-- | The identifier for the replication group.
rgReplicationGroupId :: Lens' ReplicationGroup (Maybe Text)
rgReplicationGroupId = lens _rgReplicationGroupId (\ s a -> s{_rgReplicationGroupId = a});

-- | A group of settings to be applied to the replication group, either
-- immediately or during the next maintenance window.
rgPendingModifiedValues :: Lens' ReplicationGroup (Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues = lens _rgPendingModifiedValues (\ s a -> s{_rgPendingModifiedValues = a});

-- | The description of the replication group.
rgDescription :: Lens' ReplicationGroup (Maybe Text)
rgDescription = lens _rgDescription (\ s a -> s{_rgDescription = a});

-- | Indicates the status of Multi-AZ for this replication group.
--
-- ElastiCache Multi-AZ replication groups are not supported on:
--
-- -   Redis versions earlier than 2.8.6.
-- -   T1 and T2 cache node types.
rgAutomaticFailover :: Lens' ReplicationGroup (Maybe AutomaticFailoverStatus)
rgAutomaticFailover = lens _rgAutomaticFailover (\ s a -> s{_rgAutomaticFailover = a});

instance FromXML ReplicationGroup where
        parseXML x
          = ReplicationGroup' <$>
              (x .@? "NodeGroups" .!@ mempty >>=
                 may (parseXMLList "NodeGroup"))
                <*> (x .@? "Status")
                <*> (x .@? "SnapshottingClusterId")
                <*>
                (x .@? "MemberClusters" .!@ mempty >>=
                   may (parseXMLList "ClusterId"))
                <*> (x .@? "ReplicationGroupId")
                <*> (x .@? "PendingModifiedValues")
                <*> (x .@? "Description")
                <*> (x .@? "AutomaticFailover")

-- | The settings to be applied to the replication group, either immediately
-- or during the next maintenance window.
--
-- /See:/ 'replicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
    { _rgpmvPrimaryClusterId        :: !(Maybe Text)
    , _rgpmvAutomaticFailoverStatus :: !(Maybe PendingAutomaticFailoverStatus)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReplicationGroupPendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgpmvPrimaryClusterId'
--
-- * 'rgpmvAutomaticFailoverStatus'
replicationGroupPendingModifiedValues
    :: ReplicationGroupPendingModifiedValues
replicationGroupPendingModifiedValues =
    ReplicationGroupPendingModifiedValues'
    { _rgpmvPrimaryClusterId = Nothing
    , _rgpmvAutomaticFailoverStatus = Nothing
    }

-- | The primary cluster ID which will be applied immediately (if
-- '--apply-immediately' was specified), or during the next maintenance
-- window.
rgpmvPrimaryClusterId :: Lens' ReplicationGroupPendingModifiedValues (Maybe Text)
rgpmvPrimaryClusterId = lens _rgpmvPrimaryClusterId (\ s a -> s{_rgpmvPrimaryClusterId = a});

-- | Indicates the status of Multi-AZ for this replication group.
--
-- ElastiCache Multi-AZ replication groups are not supported on:
--
-- -   Redis versions earlier than 2.8.6.
-- -   T1 and T2 cache node types.
rgpmvAutomaticFailoverStatus :: Lens' ReplicationGroupPendingModifiedValues (Maybe PendingAutomaticFailoverStatus)
rgpmvAutomaticFailoverStatus = lens _rgpmvAutomaticFailoverStatus (\ s a -> s{_rgpmvAutomaticFailoverStatus = a});

instance FromXML
         ReplicationGroupPendingModifiedValues where
        parseXML x
          = ReplicationGroupPendingModifiedValues' <$>
              (x .@? "PrimaryClusterId") <*>
                (x .@? "AutomaticFailoverStatus")

-- | Represents the output of a /PurchaseReservedCacheNodesOffering/ action.
--
-- /See:/ 'reservedCacheNode' smart constructor.
data ReservedCacheNode = ReservedCacheNode'
    { _rcnCacheNodeType                :: !(Maybe Text)
    , _rcnState                        :: !(Maybe Text)
    , _rcnProductDescription           :: !(Maybe Text)
    , _rcnStartTime                    :: !(Maybe ISO8601)
    , _rcnCacheNodeCount               :: !(Maybe Int)
    , _rcnReservedCacheNodeId          :: !(Maybe Text)
    , _rcnOfferingType                 :: !(Maybe Text)
    , _rcnUsagePrice                   :: !(Maybe Double)
    , _rcnRecurringCharges             :: !(Maybe [RecurringCharge])
    , _rcnFixedPrice                   :: !(Maybe Double)
    , _rcnDuration                     :: !(Maybe Int)
    , _rcnReservedCacheNodesOfferingId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReservedCacheNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcnCacheNodeType'
--
-- * 'rcnState'
--
-- * 'rcnProductDescription'
--
-- * 'rcnStartTime'
--
-- * 'rcnCacheNodeCount'
--
-- * 'rcnReservedCacheNodeId'
--
-- * 'rcnOfferingType'
--
-- * 'rcnUsagePrice'
--
-- * 'rcnRecurringCharges'
--
-- * 'rcnFixedPrice'
--
-- * 'rcnDuration'
--
-- * 'rcnReservedCacheNodesOfferingId'
reservedCacheNode
    :: ReservedCacheNode
reservedCacheNode =
    ReservedCacheNode'
    { _rcnCacheNodeType = Nothing
    , _rcnState = Nothing
    , _rcnProductDescription = Nothing
    , _rcnStartTime = Nothing
    , _rcnCacheNodeCount = Nothing
    , _rcnReservedCacheNodeId = Nothing
    , _rcnOfferingType = Nothing
    , _rcnUsagePrice = Nothing
    , _rcnRecurringCharges = Nothing
    , _rcnFixedPrice = Nothing
    , _rcnDuration = Nothing
    , _rcnReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type for the reserved cache nodes.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: 'cache.t2.micro', 'cache.t2.small',
--         'cache.t2.medium', 'cache.m3.medium', 'cache.m3.large',
--         'cache.m3.xlarge', 'cache.m3.2xlarge'
--     -   Previous generation: 'cache.t1.micro', 'cache.m1.small',
--         'cache.m1.medium', 'cache.m1.large', 'cache.m1.xlarge'
-- -   Compute optimized: 'cache.c1.xlarge'
-- -   Memory optimized
--     -   Current generation: 'cache.r3.large', 'cache.r3.xlarge',
--         'cache.r3.2xlarge', 'cache.r3.4xlarge', 'cache.r3.8xlarge'
--     -   Previous generation: 'cache.m2.xlarge', 'cache.m2.2xlarge',
--         'cache.m2.4xlarge'
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
rcnCacheNodeType :: Lens' ReservedCacheNode (Maybe Text)
rcnCacheNodeType = lens _rcnCacheNodeType (\ s a -> s{_rcnCacheNodeType = a});

-- | The state of the reserved cache node.
rcnState :: Lens' ReservedCacheNode (Maybe Text)
rcnState = lens _rcnState (\ s a -> s{_rcnState = a});

-- | The description of the reserved cache node.
rcnProductDescription :: Lens' ReservedCacheNode (Maybe Text)
rcnProductDescription = lens _rcnProductDescription (\ s a -> s{_rcnProductDescription = a});

-- | The time the reservation started.
rcnStartTime :: Lens' ReservedCacheNode (Maybe UTCTime)
rcnStartTime = lens _rcnStartTime (\ s a -> s{_rcnStartTime = a}) . mapping _Time;

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount :: Lens' ReservedCacheNode (Maybe Int)
rcnCacheNodeCount = lens _rcnCacheNodeCount (\ s a -> s{_rcnCacheNodeCount = a});

-- | The unique identifier for the reservation.
rcnReservedCacheNodeId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodeId = lens _rcnReservedCacheNodeId (\ s a -> s{_rcnReservedCacheNodeId = a});

-- | The offering type of this reserved cache node.
rcnOfferingType :: Lens' ReservedCacheNode (Maybe Text)
rcnOfferingType = lens _rcnOfferingType (\ s a -> s{_rcnOfferingType = a});

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice :: Lens' ReservedCacheNode (Maybe Double)
rcnUsagePrice = lens _rcnUsagePrice (\ s a -> s{_rcnUsagePrice = a});

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges :: Lens' ReservedCacheNode [RecurringCharge]
rcnRecurringCharges = lens _rcnRecurringCharges (\ s a -> s{_rcnRecurringCharges = a}) . _Default . _Coerce;

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice :: Lens' ReservedCacheNode (Maybe Double)
rcnFixedPrice = lens _rcnFixedPrice (\ s a -> s{_rcnFixedPrice = a});

-- | The duration of the reservation in seconds.
rcnDuration :: Lens' ReservedCacheNode (Maybe Int)
rcnDuration = lens _rcnDuration (\ s a -> s{_rcnDuration = a});

-- | The offering identifier.
rcnReservedCacheNodesOfferingId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodesOfferingId = lens _rcnReservedCacheNodesOfferingId (\ s a -> s{_rcnReservedCacheNodesOfferingId = a});

instance FromXML ReservedCacheNode where
        parseXML x
          = ReservedCacheNode' <$>
              (x .@? "CacheNodeType") <*> (x .@? "State") <*>
                (x .@? "ProductDescription")
                <*> (x .@? "StartTime")
                <*> (x .@? "CacheNodeCount")
                <*> (x .@? "ReservedCacheNodeId")
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")
                <*> (x .@? "ReservedCacheNodesOfferingId")

-- | Describes all of the attributes of a reserved cache node offering.
--
-- /See:/ 'reservedCacheNodesOffering' smart constructor.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering'
    { _rcnoCacheNodeType                :: !(Maybe Text)
    , _rcnoProductDescription           :: !(Maybe Text)
    , _rcnoOfferingType                 :: !(Maybe Text)
    , _rcnoUsagePrice                   :: !(Maybe Double)
    , _rcnoRecurringCharges             :: !(Maybe [RecurringCharge])
    , _rcnoFixedPrice                   :: !(Maybe Double)
    , _rcnoDuration                     :: !(Maybe Int)
    , _rcnoReservedCacheNodesOfferingId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReservedCacheNodesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcnoCacheNodeType'
--
-- * 'rcnoProductDescription'
--
-- * 'rcnoOfferingType'
--
-- * 'rcnoUsagePrice'
--
-- * 'rcnoRecurringCharges'
--
-- * 'rcnoFixedPrice'
--
-- * 'rcnoDuration'
--
-- * 'rcnoReservedCacheNodesOfferingId'
reservedCacheNodesOffering
    :: ReservedCacheNodesOffering
reservedCacheNodesOffering =
    ReservedCacheNodesOffering'
    { _rcnoCacheNodeType = Nothing
    , _rcnoProductDescription = Nothing
    , _rcnoOfferingType = Nothing
    , _rcnoUsagePrice = Nothing
    , _rcnoRecurringCharges = Nothing
    , _rcnoFixedPrice = Nothing
    , _rcnoDuration = Nothing
    , _rcnoReservedCacheNodesOfferingId = Nothing
    }

-- | The cache node type for the reserved cache node.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: 'cache.t2.micro', 'cache.t2.small',
--         'cache.t2.medium', 'cache.m3.medium', 'cache.m3.large',
--         'cache.m3.xlarge', 'cache.m3.2xlarge'
--     -   Previous generation: 'cache.t1.micro', 'cache.m1.small',
--         'cache.m1.medium', 'cache.m1.large', 'cache.m1.xlarge'
-- -   Compute optimized: 'cache.c1.xlarge'
-- -   Memory optimized
--     -   Current generation: 'cache.r3.large', 'cache.r3.xlarge',
--         'cache.r3.2xlarge', 'cache.r3.4xlarge', 'cache.r3.8xlarge'
--     -   Previous generation: 'cache.m2.xlarge', 'cache.m2.2xlarge',
--         'cache.m2.4xlarge'
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
rcnoCacheNodeType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoCacheNodeType = lens _rcnoCacheNodeType (\ s a -> s{_rcnoCacheNodeType = a});

-- | The cache engine used by the offering.
rcnoProductDescription :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoProductDescription = lens _rcnoProductDescription (\ s a -> s{_rcnoProductDescription = a});

-- | The offering type.
rcnoOfferingType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoOfferingType = lens _rcnoOfferingType (\ s a -> s{_rcnoOfferingType = a});

-- | The hourly price charged for this offering.
rcnoUsagePrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoUsagePrice = lens _rcnoUsagePrice (\ s a -> s{_rcnoUsagePrice = a});

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges :: Lens' ReservedCacheNodesOffering [RecurringCharge]
rcnoRecurringCharges = lens _rcnoRecurringCharges (\ s a -> s{_rcnoRecurringCharges = a}) . _Default . _Coerce;

-- | The fixed price charged for this offering.
rcnoFixedPrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoFixedPrice = lens _rcnoFixedPrice (\ s a -> s{_rcnoFixedPrice = a});

-- | The duration of the offering. in seconds.
rcnoDuration :: Lens' ReservedCacheNodesOffering (Maybe Int)
rcnoDuration = lens _rcnoDuration (\ s a -> s{_rcnoDuration = a});

-- | A unique identifier for the reserved cache node offering.
rcnoReservedCacheNodesOfferingId :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoReservedCacheNodesOfferingId = lens _rcnoReservedCacheNodesOfferingId (\ s a -> s{_rcnoReservedCacheNodesOfferingId = a});

instance FromXML ReservedCacheNodesOffering where
        parseXML x
          = ReservedCacheNodesOffering' <$>
              (x .@? "CacheNodeType") <*>
                (x .@? "ProductDescription")
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")
                <*> (x .@? "ReservedCacheNodesOfferingId")

-- | Represents a single cache security group and its status.
--
-- /See:/ 'securityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
    { _sgmStatus          :: !(Maybe Text)
    , _sgmSecurityGroupId :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'SecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgmStatus'
--
-- * 'sgmSecurityGroupId'
securityGroupMembership
    :: SecurityGroupMembership
securityGroupMembership =
    SecurityGroupMembership'
    { _sgmStatus = Nothing
    , _sgmSecurityGroupId = Nothing
    }

-- | The status of the cache security group membership. The status changes
-- whenever a cache security group is modified, or when the cache security
-- groups assigned to a cache cluster are modified.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus = lens _sgmStatus (\ s a -> s{_sgmStatus = a});

-- | The identifier of the cache security group.
sgmSecurityGroupId :: Lens' SecurityGroupMembership (Maybe Text)
sgmSecurityGroupId = lens _sgmSecurityGroupId (\ s a -> s{_sgmSecurityGroupId = a});

instance FromXML SecurityGroupMembership where
        parseXML x
          = SecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "SecurityGroupId")

-- | Represents a copy of an entire cache cluster as of the time when the
-- snapshot was taken.
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
    { _sCacheNodeType              :: !(Maybe Text)
    , _sEngineVersion              :: !(Maybe Text)
    , _sCacheClusterCreateTime     :: !(Maybe ISO8601)
    , _sAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _sCacheParameterGroupName    :: !(Maybe Text)
    , _sSnapshotStatus             :: !(Maybe Text)
    , _sSnapshotWindow             :: !(Maybe Text)
    , _sVPCId                      :: !(Maybe Text)
    , _sCacheClusterId             :: !(Maybe Text)
    , _sEngine                     :: !(Maybe Text)
    , _sPreferredMaintenanceWindow :: !(Maybe Text)
    , _sTopicARN                   :: !(Maybe Text)
    , _sCacheSubnetGroupName       :: !(Maybe Text)
    , _sNodeSnapshots              :: !(Maybe [NodeSnapshot])
    , _sPreferredAvailabilityZone  :: !(Maybe Text)
    , _sSnapshotRetentionLimit     :: !(Maybe Int)
    , _sSnapshotName               :: !(Maybe Text)
    , _sSnapshotSource             :: !(Maybe Text)
    , _sNumCacheNodes              :: !(Maybe Int)
    , _sPort                       :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sCacheNodeType'
--
-- * 'sEngineVersion'
--
-- * 'sCacheClusterCreateTime'
--
-- * 'sAutoMinorVersionUpgrade'
--
-- * 'sCacheParameterGroupName'
--
-- * 'sSnapshotStatus'
--
-- * 'sSnapshotWindow'
--
-- * 'sVPCId'
--
-- * 'sCacheClusterId'
--
-- * 'sEngine'
--
-- * 'sPreferredMaintenanceWindow'
--
-- * 'sTopicARN'
--
-- * 'sCacheSubnetGroupName'
--
-- * 'sNodeSnapshots'
--
-- * 'sPreferredAvailabilityZone'
--
-- * 'sSnapshotRetentionLimit'
--
-- * 'sSnapshotName'
--
-- * 'sSnapshotSource'
--
-- * 'sNumCacheNodes'
--
-- * 'sPort'
snapshot
    :: Snapshot
snapshot =
    Snapshot'
    { _sCacheNodeType = Nothing
    , _sEngineVersion = Nothing
    , _sCacheClusterCreateTime = Nothing
    , _sAutoMinorVersionUpgrade = Nothing
    , _sCacheParameterGroupName = Nothing
    , _sSnapshotStatus = Nothing
    , _sSnapshotWindow = Nothing
    , _sVPCId = Nothing
    , _sCacheClusterId = Nothing
    , _sEngine = Nothing
    , _sPreferredMaintenanceWindow = Nothing
    , _sTopicARN = Nothing
    , _sCacheSubnetGroupName = Nothing
    , _sNodeSnapshots = Nothing
    , _sPreferredAvailabilityZone = Nothing
    , _sSnapshotRetentionLimit = Nothing
    , _sSnapshotName = Nothing
    , _sSnapshotSource = Nothing
    , _sNumCacheNodes = Nothing
    , _sPort = Nothing
    }

-- | The name of the compute and memory capacity node type for the source
-- cache cluster.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: 'cache.t2.micro', 'cache.t2.small',
--         'cache.t2.medium', 'cache.m3.medium', 'cache.m3.large',
--         'cache.m3.xlarge', 'cache.m3.2xlarge'
--     -   Previous generation: 'cache.t1.micro', 'cache.m1.small',
--         'cache.m1.medium', 'cache.m1.large', 'cache.m1.xlarge'
-- -   Compute optimized: 'cache.c1.xlarge'
-- -   Memory optimized
--     -   Current generation: 'cache.r3.large', 'cache.r3.xlarge',
--         'cache.r3.2xlarge', 'cache.r3.4xlarge', 'cache.r3.8xlarge'
--     -   Previous generation: 'cache.m2.xlarge', 'cache.m2.2xlarge',
--         'cache.m2.4xlarge'
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
sCacheNodeType :: Lens' Snapshot (Maybe Text)
sCacheNodeType = lens _sCacheNodeType (\ s a -> s{_sCacheNodeType = a});

-- | The version of the cache engine version that is used by the source cache
-- cluster.
sEngineVersion :: Lens' Snapshot (Maybe Text)
sEngineVersion = lens _sEngineVersion (\ s a -> s{_sEngineVersion = a});

-- | The date and time when the source cache cluster was created.
sCacheClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
sCacheClusterCreateTime = lens _sCacheClusterCreateTime (\ s a -> s{_sCacheClusterCreateTime = a}) . mapping _Time;

-- | This parameter is currently disabled.
sAutoMinorVersionUpgrade :: Lens' Snapshot (Maybe Bool)
sAutoMinorVersionUpgrade = lens _sAutoMinorVersionUpgrade (\ s a -> s{_sAutoMinorVersionUpgrade = a});

-- | The cache parameter group that is associated with the source cache
-- cluster.
sCacheParameterGroupName :: Lens' Snapshot (Maybe Text)
sCacheParameterGroupName = lens _sCacheParameterGroupName (\ s a -> s{_sCacheParameterGroupName = a});

-- | The status of the snapshot. Valid values: 'creating' | 'available' |
-- 'restoring' | 'copying' | 'deleting'.
sSnapshotStatus :: Lens' Snapshot (Maybe Text)
sSnapshotStatus = lens _sSnapshotStatus (\ s a -> s{_sSnapshotStatus = a});

-- | The daily time range during which ElastiCache takes daily snapshots of
-- the source cache cluster.
sSnapshotWindow :: Lens' Snapshot (Maybe Text)
sSnapshotWindow = lens _sSnapshotWindow (\ s a -> s{_sSnapshotWindow = a});

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet
-- group for the source cache cluster.
sVPCId :: Lens' Snapshot (Maybe Text)
sVPCId = lens _sVPCId (\ s a -> s{_sVPCId = a});

-- | The user-supplied identifier of the source cache cluster.
sCacheClusterId :: Lens' Snapshot (Maybe Text)
sCacheClusterId = lens _sCacheClusterId (\ s a -> s{_sCacheClusterId = a});

-- | The name of the cache engine (/memcached/ or /redis/) used by the source
-- cache cluster.
sEngine :: Lens' Snapshot (Maybe Text)
sEngine = lens _sEngine (\ s a -> s{_sEngine = a});

-- | Specifies the weekly time range during which maintenance on the cache
-- cluster is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for 'ddd' are:
--
-- -   'sun'
-- -   'mon'
-- -   'tue'
-- -   'wed'
-- -   'thu'
-- -   'fri'
-- -   'sat'
--
-- Example: 'sun:05:00-sun:09:00'
sPreferredMaintenanceWindow :: Lens' Snapshot (Maybe Text)
sPreferredMaintenanceWindow = lens _sPreferredMaintenanceWindow (\ s a -> s{_sPreferredMaintenanceWindow = a});

-- | The Amazon Resource Name (ARN) for the topic used by the source cache
-- cluster for publishing notifications.
sTopicARN :: Lens' Snapshot (Maybe Text)
sTopicARN = lens _sTopicARN (\ s a -> s{_sTopicARN = a});

-- | The name of the cache subnet group associated with the source cache
-- cluster.
sCacheSubnetGroupName :: Lens' Snapshot (Maybe Text)
sCacheSubnetGroupName = lens _sCacheSubnetGroupName (\ s a -> s{_sCacheSubnetGroupName = a});

-- | A list of the cache nodes in the source cache cluster.
sNodeSnapshots :: Lens' Snapshot [NodeSnapshot]
sNodeSnapshots = lens _sNodeSnapshots (\ s a -> s{_sNodeSnapshots = a}) . _Default . _Coerce;

-- | The name of the Availability Zone in which the source cache cluster is
-- located.
sPreferredAvailabilityZone :: Lens' Snapshot (Maybe Text)
sPreferredAvailabilityZone = lens _sPreferredAvailabilityZone (\ s a -> s{_sPreferredAvailabilityZone = a});

-- | For an automatic snapshot, the number of days for which ElastiCache will
-- retain the snapshot before deleting it.
--
-- For manual snapshots, this field reflects the /SnapshotRetentionLimit/
-- for the source cache cluster when the snapshot was created. This field
-- is otherwise ignored: Manual snapshots do not expire, and can only be
-- deleted using the /DeleteSnapshot/ action.
--
-- __Important__
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
sSnapshotRetentionLimit :: Lens' Snapshot (Maybe Int)
sSnapshotRetentionLimit = lens _sSnapshotRetentionLimit (\ s a -> s{_sSnapshotRetentionLimit = a});

-- | The name of a snapshot. For an automatic snapshot, the name is
-- system-generated; for a manual snapshot, this is the user-provided name.
sSnapshotName :: Lens' Snapshot (Maybe Text)
sSnapshotName = lens _sSnapshotName (\ s a -> s{_sSnapshotName = a});

-- | Indicates whether the snapshot is from an automatic backup ('automated')
-- or was created manually ('manual').
sSnapshotSource :: Lens' Snapshot (Maybe Text)
sSnapshotSource = lens _sSnapshotSource (\ s a -> s{_sSnapshotSource = a});

-- | The number of cache nodes in the source cache cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
sNumCacheNodes :: Lens' Snapshot (Maybe Int)
sNumCacheNodes = lens _sNumCacheNodes (\ s a -> s{_sNumCacheNodes = a});

-- | The port number used by each cache nodes in the source cache cluster.
sPort :: Lens' Snapshot (Maybe Int)
sPort = lens _sPort (\ s a -> s{_sPort = a});

instance FromXML Snapshot where
        parseXML x
          = Snapshot' <$>
              (x .@? "CacheNodeType") <*> (x .@? "EngineVersion")
                <*> (x .@? "CacheClusterCreateTime")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*> (x .@? "CacheParameterGroupName")
                <*> (x .@? "SnapshotStatus")
                <*> (x .@? "SnapshotWindow")
                <*> (x .@? "VpcId")
                <*> (x .@? "CacheClusterId")
                <*> (x .@? "Engine")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "TopicArn")
                <*> (x .@? "CacheSubnetGroupName")
                <*>
                (x .@? "NodeSnapshots" .!@ mempty >>=
                   may (parseXMLList "NodeSnapshot"))
                <*> (x .@? "PreferredAvailabilityZone")
                <*> (x .@? "SnapshotRetentionLimit")
                <*> (x .@? "SnapshotName")
                <*> (x .@? "SnapshotSource")
                <*> (x .@? "NumCacheNodes")
                <*> (x .@? "Port")

-- | Represents the subnet associated with a cache cluster. This parameter
-- refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC)
-- and used with ElastiCache.
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
    { _sSubnetIdentifier       :: !(Maybe Text)
    , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetIdentifier'
--
-- * 'sSubnetAvailabilityZone'
subnet
    :: Subnet
subnet =
    Subnet'
    { _sSubnetIdentifier = Nothing
    , _sSubnetAvailabilityZone = Nothing
    }

-- | The unique identifier for the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a});

-- | The Availability Zone associated with the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a});

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetIdentifier") <*>
                (x .@? "SubnetAvailabilityZone")

-- | A cost allocation Tag that can be added to an ElastiCache cluster or
-- replication group. Tags are composed of a Key\/Value pair. A tag with a
-- null Value is permitted.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The tag\'s value. May not be null.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key for the tag.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | Represents the output from the /AddTagsToResource/,
-- /ListTagsOnResource/, and /RemoveTagsFromResource/ actions.
--
-- /See:/ 'tagListMessage' smart constructor.
newtype TagListMessage = TagListMessage'
    { _tlmTagList :: Maybe [Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TagListMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlmTagList'
tagListMessage
    :: TagListMessage
tagListMessage =
    TagListMessage'
    { _tlmTagList = Nothing
    }

-- | A list of cost allocation tags as key-value pairs.
tlmTagList :: Lens' TagListMessage [Tag]
tlmTagList = lens _tlmTagList (\ s a -> s{_tlmTagList = a}) . _Default . _Coerce;

instance FromXML TagListMessage where
        parseXML x
          = TagListMessage' <$>
              (x .@? "TagList" .!@ mempty >>=
                 may (parseXMLList "Tag"))
