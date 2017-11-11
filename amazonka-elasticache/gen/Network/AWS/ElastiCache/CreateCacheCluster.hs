{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheCluster
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache cluster. All nodes in the cache cluster run the same protocol-compliant cache engine software, either Memcached or Redis.
--
--
-- /Important:/ Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups.
--
module Network.AWS.ElastiCache.CreateCacheCluster
    (
    -- * Creating a Request
      createCacheCluster
    , CreateCacheCluster
    -- * Request Lenses
    , cccEngineVersion
    , cccCacheNodeType
    , cccSecurityGroupIds
    , cccSnapshotARNs
    , cccAutoMinorVersionUpgrade
    , cccCacheParameterGroupName
    , cccSnapshotWindow
    , cccAuthToken
    , cccEngine
    , cccPreferredAvailabilityZones
    , cccPreferredMaintenanceWindow
    , cccCacheSubnetGroupName
    , cccPreferredAvailabilityZone
    , cccSnapshotRetentionLimit
    , cccAZMode
    , cccSnapshotName
    , cccReplicationGroupId
    , cccNotificationTopicARN
    , cccNumCacheNodes
    , cccTags
    , cccPort
    , cccCacheSecurityGroupNames
    , cccCacheClusterId

    -- * Destructuring the Response
    , createCacheClusterResponse
    , CreateCacheClusterResponse
    -- * Response Lenses
    , cccrsCacheCluster
    , cccrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a CreateCacheCluster operation.
--
--
--
-- /See:/ 'createCacheCluster' smart constructor.
data CreateCacheCluster = CreateCacheCluster'
  { _cccEngineVersion              :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccCacheNodeType              :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccSecurityGroupIds           :: {-# NOUNPACK #-}!(Maybe [Text])
  , _cccSnapshotARNs               :: {-# NOUNPACK #-}!(Maybe [Text])
  , _cccAutoMinorVersionUpgrade    :: {-# NOUNPACK #-}!(Maybe Bool)
  , _cccCacheParameterGroupName    :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccSnapshotWindow             :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccAuthToken                  :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccEngine                     :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccPreferredAvailabilityZones :: {-# NOUNPACK #-}!(Maybe [Text])
  , _cccPreferredMaintenanceWindow :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccCacheSubnetGroupName       :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccPreferredAvailabilityZone  :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccSnapshotRetentionLimit     :: {-# NOUNPACK #-}!(Maybe Int)
  , _cccAZMode                     :: {-# NOUNPACK #-}!(Maybe AZMode)
  , _cccSnapshotName               :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccReplicationGroupId         :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccNotificationTopicARN       :: {-# NOUNPACK #-}!(Maybe Text)
  , _cccNumCacheNodes              :: {-# NOUNPACK #-}!(Maybe Int)
  , _cccTags                       :: {-# NOUNPACK #-}!(Maybe [Tag])
  , _cccPort                       :: {-# NOUNPACK #-}!(Maybe Int)
  , _cccCacheSecurityGroupNames    :: {-# NOUNPACK #-}!(Maybe [Text])
  , _cccCacheClusterId             :: {-# NOUNPACK #-}!Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCacheCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccEngineVersion' - The version number of the cache engine to be used for this cache cluster. To view the supported cache engine versions, use the DescribeCacheEngineVersions operation. __Important:__ You can upgrade to a newer engine version (see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cache cluster or replication group and create it anew with the earlier engine version.
--
-- * 'cccCacheNodeType' - The compute and memory capacity of the nodes in the node group (shard). Valid node types are as follows:     * General purpose:     * Current generation: @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@ , @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@ , @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: @cache.t1.micro@ , @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized: @cache.c1.xlarge@      * Memory optimized:     * Current generation: @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis backup/restore is not supported for Redis (cluster mode disabled) T1 and T2 instances. Backup/restore is supported on Redis (cluster mode enabled) T2 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
-- * 'cccSecurityGroupIds' - One or more VPC security groups associated with the cache cluster. Use this parameter only when you are creating a cache cluster in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- * 'cccSnapshotARNs' - A single-element string list containing an Amazon Resource Name (ARN) that uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The snapshot file is used to populate the node group (shard). The Amazon S3 object name in the ARN cannot contain any commas. Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
--
-- * 'cccAutoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- * 'cccCacheParameterGroupName' - The name of the parameter group to associate with this cache cluster. If this argument is omitted, the default parameter group for the specified engine is used. You cannot use any parameter group which has @cluster-enabled='yes'@ when creating a cluster.
--
-- * 'cccSnapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range. __Note:__ This parameter is only valid if the @Engine@ parameter is @redis@ .
--
-- * 'cccAuthToken' - __Reserved parameter.__ The password used to access a password protected server. Password constraints:     * Must be only printable ASCII characters.     * Must be at least 16 characters and no more than 128 characters in length.     * Cannot contain any of the following characters: '/', '"', or "@".  For more information, see <http://redis.io/commands/AUTH AUTH password> at Redis.
--
-- * 'cccEngine' - The name of the cache engine to be used for this cache cluster. Valid values for this parameter are: @memcached@ | @redis@
--
-- * 'cccPreferredAvailabilityZones' - A list of the Availability Zones in which cache nodes are created. The order of the zones in the list is not important. This option is only supported on Memcached. If you want all the nodes in the same Availability Zone, use @PreferredAvailabilityZone@ instead, or repeat the Availability Zone multiple times in the list. Default: System chosen Availability Zones.
--
-- * 'cccPreferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cache cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are: Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
--
-- * 'cccCacheSubnetGroupName' - The name of the subnet group to be used for the cache cluster. Use this parameter only when you are creating a cache cluster in an Amazon Virtual Private Cloud (Amazon VPC). /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SubnetGroups.html Subnets and Subnet Groups> .
--
-- * 'cccPreferredAvailabilityZone' - The EC2 Availability Zone in which the cache cluster is created. All nodes belonging to this Memcached cache cluster are placed in the preferred Availability Zone. If you want to create your nodes across multiple Availability Zones, use @PreferredAvailabilityZones@ . Default: System chosen Availability Zone.
--
-- * 'cccSnapshotRetentionLimit' - The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot taken today is retained for 5 days before being deleted. Default: 0 (i.e., automatic backups are disabled for this cache cluster).
--
-- * 'cccAZMode' - Specifies whether the nodes in this Memcached cluster are created in a single Availability Zone or created across multiple Availability Zones in the cluster's region. This parameter is only supported for Memcached cache clusters. If the @AZMode@ and @PreferredAvailabilityZones@ are not specified, ElastiCache assumes @single-az@ mode.
--
-- * 'cccSnapshotName' - The name of a Redis snapshot from which to restore data into the new node group (shard). The snapshot status changes to @restoring@ while the new node group (shard) is being created.
--
-- * 'cccReplicationGroupId' - /Important:/ Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups. The ID of the replication group to which this cache cluster should belong. If this parameter is specified, the cache cluster is added to the specified replication group as a read replica; otherwise, the cache cluster is a standalone primary that is not part of any replication group. If the specified replication group is Multi-AZ enabled and the Availability Zone is not specified, the cache cluster is created in Availability Zones that provide the best spread of read replicas across Availability Zones.
--
-- * 'cccNotificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
--
-- * 'cccNumCacheNodes' - The initial number of cache nodes that the cache cluster has. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20. If you need more than 20 nodes for your Memcached cluster, please fill out the ElastiCache Limit Increase Request form at <http://aws.amazon.com/contact-us/elasticache-node-limit-request/ http://aws.amazon.com/contact-us/elasticache-node-limit-request/> .
--
-- * 'cccTags' - A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
--
-- * 'cccPort' - The port number on which each of the cache nodes accepts connections.
--
-- * 'cccCacheSecurityGroupNames' - A list of security group names to associate with this cache cluster. Use this parameter only when you are creating a cache cluster outside of an Amazon Virtual Private Cloud (Amazon VPC).
--
-- * 'cccCacheClusterId' - The node group (shard) identifier. This parameter is stored as a lowercase string. __Constraints:__      * A name must contain from 1 to 20 alphanumeric characters or hyphens.     * The first character must be a letter.     * A name cannot end with a hyphen or contain two consecutive hyphens.
createCacheCluster
    :: Text -- ^ 'cccCacheClusterId'
    -> CreateCacheCluster
createCacheCluster pCacheClusterId_ =
  CreateCacheCluster'
  { _cccEngineVersion = Nothing
  , _cccCacheNodeType = Nothing
  , _cccSecurityGroupIds = Nothing
  , _cccSnapshotARNs = Nothing
  , _cccAutoMinorVersionUpgrade = Nothing
  , _cccCacheParameterGroupName = Nothing
  , _cccSnapshotWindow = Nothing
  , _cccAuthToken = Nothing
  , _cccEngine = Nothing
  , _cccPreferredAvailabilityZones = Nothing
  , _cccPreferredMaintenanceWindow = Nothing
  , _cccCacheSubnetGroupName = Nothing
  , _cccPreferredAvailabilityZone = Nothing
  , _cccSnapshotRetentionLimit = Nothing
  , _cccAZMode = Nothing
  , _cccSnapshotName = Nothing
  , _cccReplicationGroupId = Nothing
  , _cccNotificationTopicARN = Nothing
  , _cccNumCacheNodes = Nothing
  , _cccTags = Nothing
  , _cccPort = Nothing
  , _cccCacheSecurityGroupNames = Nothing
  , _cccCacheClusterId = pCacheClusterId_
  }


-- | The version number of the cache engine to be used for this cache cluster. To view the supported cache engine versions, use the DescribeCacheEngineVersions operation. __Important:__ You can upgrade to a newer engine version (see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cache cluster or replication group and create it anew with the earlier engine version.
cccEngineVersion :: Lens' CreateCacheCluster (Maybe Text)
cccEngineVersion = lens _cccEngineVersion (\ s a -> s{_cccEngineVersion = a});

-- | The compute and memory capacity of the nodes in the node group (shard). Valid node types are as follows:     * General purpose:     * Current generation: @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@ , @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@ , @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: @cache.t1.micro@ , @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized: @cache.c1.xlarge@      * Memory optimized:     * Current generation: @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis backup/restore is not supported for Redis (cluster mode disabled) T1 and T2 instances. Backup/restore is supported on Redis (cluster mode enabled) T2 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
cccCacheNodeType :: Lens' CreateCacheCluster (Maybe Text)
cccCacheNodeType = lens _cccCacheNodeType (\ s a -> s{_cccCacheNodeType = a});

-- | One or more VPC security groups associated with the cache cluster. Use this parameter only when you are creating a cache cluster in an Amazon Virtual Private Cloud (Amazon VPC).
cccSecurityGroupIds :: Lens' CreateCacheCluster [Text]
cccSecurityGroupIds = lens _cccSecurityGroupIds (\ s a -> s{_cccSecurityGroupIds = a}) . _Default . _Coerce;

-- | A single-element string list containing an Amazon Resource Name (ARN) that uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The snapshot file is used to populate the node group (shard). The Amazon S3 object name in the ARN cannot contain any commas. Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
cccSnapshotARNs :: Lens' CreateCacheCluster [Text]
cccSnapshotARNs = lens _cccSnapshotARNs (\ s a -> s{_cccSnapshotARNs = a}) . _Default . _Coerce;

-- | This parameter is currently disabled.
cccAutoMinorVersionUpgrade :: Lens' CreateCacheCluster (Maybe Bool)
cccAutoMinorVersionUpgrade = lens _cccAutoMinorVersionUpgrade (\ s a -> s{_cccAutoMinorVersionUpgrade = a});

-- | The name of the parameter group to associate with this cache cluster. If this argument is omitted, the default parameter group for the specified engine is used. You cannot use any parameter group which has @cluster-enabled='yes'@ when creating a cluster.
cccCacheParameterGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheParameterGroupName = lens _cccCacheParameterGroupName (\ s a -> s{_cccCacheParameterGroupName = a});

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range. __Note:__ This parameter is only valid if the @Engine@ parameter is @redis@ .
cccSnapshotWindow :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotWindow = lens _cccSnapshotWindow (\ s a -> s{_cccSnapshotWindow = a});

-- | __Reserved parameter.__ The password used to access a password protected server. Password constraints:     * Must be only printable ASCII characters.     * Must be at least 16 characters and no more than 128 characters in length.     * Cannot contain any of the following characters: '/', '"', or "@".  For more information, see <http://redis.io/commands/AUTH AUTH password> at Redis.
cccAuthToken :: Lens' CreateCacheCluster (Maybe Text)
cccAuthToken = lens _cccAuthToken (\ s a -> s{_cccAuthToken = a});

-- | The name of the cache engine to be used for this cache cluster. Valid values for this parameter are: @memcached@ | @redis@
cccEngine :: Lens' CreateCacheCluster (Maybe Text)
cccEngine = lens _cccEngine (\ s a -> s{_cccEngine = a});

-- | A list of the Availability Zones in which cache nodes are created. The order of the zones in the list is not important. This option is only supported on Memcached. If you want all the nodes in the same Availability Zone, use @PreferredAvailabilityZone@ instead, or repeat the Availability Zone multiple times in the list. Default: System chosen Availability Zones.
cccPreferredAvailabilityZones :: Lens' CreateCacheCluster [Text]
cccPreferredAvailabilityZones = lens _cccPreferredAvailabilityZones (\ s a -> s{_cccPreferredAvailabilityZones = a}) . _Default . _Coerce;

-- | Specifies the weekly time range during which maintenance on the cache cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are: Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
cccPreferredMaintenanceWindow :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredMaintenanceWindow = lens _cccPreferredMaintenanceWindow (\ s a -> s{_cccPreferredMaintenanceWindow = a});

-- | The name of the subnet group to be used for the cache cluster. Use this parameter only when you are creating a cache cluster in an Amazon Virtual Private Cloud (Amazon VPC). /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SubnetGroups.html Subnets and Subnet Groups> .
cccCacheSubnetGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheSubnetGroupName = lens _cccCacheSubnetGroupName (\ s a -> s{_cccCacheSubnetGroupName = a});

-- | The EC2 Availability Zone in which the cache cluster is created. All nodes belonging to this Memcached cache cluster are placed in the preferred Availability Zone. If you want to create your nodes across multiple Availability Zones, use @PreferredAvailabilityZones@ . Default: System chosen Availability Zone.
cccPreferredAvailabilityZone :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredAvailabilityZone = lens _cccPreferredAvailabilityZone (\ s a -> s{_cccPreferredAvailabilityZone = a});

-- | The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot taken today is retained for 5 days before being deleted. Default: 0 (i.e., automatic backups are disabled for this cache cluster).
cccSnapshotRetentionLimit :: Lens' CreateCacheCluster (Maybe Int)
cccSnapshotRetentionLimit = lens _cccSnapshotRetentionLimit (\ s a -> s{_cccSnapshotRetentionLimit = a});

-- | Specifies whether the nodes in this Memcached cluster are created in a single Availability Zone or created across multiple Availability Zones in the cluster's region. This parameter is only supported for Memcached cache clusters. If the @AZMode@ and @PreferredAvailabilityZones@ are not specified, ElastiCache assumes @single-az@ mode.
cccAZMode :: Lens' CreateCacheCluster (Maybe AZMode)
cccAZMode = lens _cccAZMode (\ s a -> s{_cccAZMode = a});

-- | The name of a Redis snapshot from which to restore data into the new node group (shard). The snapshot status changes to @restoring@ while the new node group (shard) is being created.
cccSnapshotName :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotName = lens _cccSnapshotName (\ s a -> s{_cccSnapshotName = a});

-- | /Important:/ Due to current limitations on Redis (cluster mode disabled), this operation or parameter is not supported on Redis (cluster mode enabled) replication groups. The ID of the replication group to which this cache cluster should belong. If this parameter is specified, the cache cluster is added to the specified replication group as a read replica; otherwise, the cache cluster is a standalone primary that is not part of any replication group. If the specified replication group is Multi-AZ enabled and the Availability Zone is not specified, the cache cluster is created in Availability Zones that provide the best spread of read replicas across Availability Zones.
cccReplicationGroupId :: Lens' CreateCacheCluster (Maybe Text)
cccReplicationGroupId = lens _cccReplicationGroupId (\ s a -> s{_cccReplicationGroupId = a});

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
cccNotificationTopicARN :: Lens' CreateCacheCluster (Maybe Text)
cccNotificationTopicARN = lens _cccNotificationTopicARN (\ s a -> s{_cccNotificationTopicARN = a});

-- | The initial number of cache nodes that the cache cluster has. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20. If you need more than 20 nodes for your Memcached cluster, please fill out the ElastiCache Limit Increase Request form at <http://aws.amazon.com/contact-us/elasticache-node-limit-request/ http://aws.amazon.com/contact-us/elasticache-node-limit-request/> .
cccNumCacheNodes :: Lens' CreateCacheCluster (Maybe Int)
cccNumCacheNodes = lens _cccNumCacheNodes (\ s a -> s{_cccNumCacheNodes = a});

-- | A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key must be accompanied by a tag value.
cccTags :: Lens' CreateCacheCluster [Tag]
cccTags = lens _cccTags (\ s a -> s{_cccTags = a}) . _Default . _Coerce;

-- | The port number on which each of the cache nodes accepts connections.
cccPort :: Lens' CreateCacheCluster (Maybe Int)
cccPort = lens _cccPort (\ s a -> s{_cccPort = a});

-- | A list of security group names to associate with this cache cluster. Use this parameter only when you are creating a cache cluster outside of an Amazon Virtual Private Cloud (Amazon VPC).
cccCacheSecurityGroupNames :: Lens' CreateCacheCluster [Text]
cccCacheSecurityGroupNames = lens _cccCacheSecurityGroupNames (\ s a -> s{_cccCacheSecurityGroupNames = a}) . _Default . _Coerce;

-- | The node group (shard) identifier. This parameter is stored as a lowercase string. __Constraints:__      * A name must contain from 1 to 20 alphanumeric characters or hyphens.     * The first character must be a letter.     * A name cannot end with a hyphen or contain two consecutive hyphens.
cccCacheClusterId :: Lens' CreateCacheCluster Text
cccCacheClusterId = lens _cccCacheClusterId (\ s a -> s{_cccCacheClusterId = a});

instance AWSRequest CreateCacheCluster where
        type Rs CreateCacheCluster =
             CreateCacheClusterResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "CreateCacheClusterResult"
              (\ s h x ->
                 CreateCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

instance Hashable CreateCacheCluster where

instance NFData CreateCacheCluster where

instance ToHeaders CreateCacheCluster where
        toHeaders = const mempty

instance ToPath CreateCacheCluster where
        toPath = const "/"

instance ToQuery CreateCacheCluster where
        toQuery CreateCacheCluster'{..}
          = mconcat
              ["Action" =: ("CreateCacheCluster" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "EngineVersion" =: _cccEngineVersion,
               "CacheNodeType" =: _cccCacheNodeType,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _cccSecurityGroupIds),
               "SnapshotArns" =:
                 toQuery
                   (toQueryList "SnapshotArn" <$> _cccSnapshotARNs),
               "AutoMinorVersionUpgrade" =:
                 _cccAutoMinorVersionUpgrade,
               "CacheParameterGroupName" =:
                 _cccCacheParameterGroupName,
               "SnapshotWindow" =: _cccSnapshotWindow,
               "AuthToken" =: _cccAuthToken, "Engine" =: _cccEngine,
               "PreferredAvailabilityZones" =:
                 toQuery
                   (toQueryList "PreferredAvailabilityZone" <$>
                      _cccPreferredAvailabilityZones),
               "PreferredMaintenanceWindow" =:
                 _cccPreferredMaintenanceWindow,
               "CacheSubnetGroupName" =: _cccCacheSubnetGroupName,
               "PreferredAvailabilityZone" =:
                 _cccPreferredAvailabilityZone,
               "SnapshotRetentionLimit" =:
                 _cccSnapshotRetentionLimit,
               "AZMode" =: _cccAZMode,
               "SnapshotName" =: _cccSnapshotName,
               "ReplicationGroupId" =: _cccReplicationGroupId,
               "NotificationTopicArn" =: _cccNotificationTopicARN,
               "NumCacheNodes" =: _cccNumCacheNodes,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cccTags),
               "Port" =: _cccPort,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _cccCacheSecurityGroupNames),
               "CacheClusterId" =: _cccCacheClusterId]

-- | /See:/ 'createCacheClusterResponse' smart constructor.
data CreateCacheClusterResponse = CreateCacheClusterResponse'
  { _cccrsCacheCluster   :: {-# NOUNPACK #-}!(Maybe CacheCluster)
  , _cccrsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCacheClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cccrsCacheCluster' - Undocumented member.
--
-- * 'cccrsResponseStatus' - -- | The response status code.
createCacheClusterResponse
    :: Int -- ^ 'cccrsResponseStatus'
    -> CreateCacheClusterResponse
createCacheClusterResponse pResponseStatus_ =
  CreateCacheClusterResponse'
  {_cccrsCacheCluster = Nothing, _cccrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cccrsCacheCluster :: Lens' CreateCacheClusterResponse (Maybe CacheCluster)
cccrsCacheCluster = lens _cccrsCacheCluster (\ s a -> s{_cccrsCacheCluster = a});

-- | -- | The response status code.
cccrsResponseStatus :: Lens' CreateCacheClusterResponse Int
cccrsResponseStatus = lens _cccrsResponseStatus (\ s a -> s{_cccrsResponseStatus = a});

instance NFData CreateCacheClusterResponse where
