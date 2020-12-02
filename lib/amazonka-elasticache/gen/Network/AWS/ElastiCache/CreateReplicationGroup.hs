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
-- Module      : Network.AWS.ElastiCache.CreateReplicationGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group.
--
--
-- A Redis (cluster mode disabled) replication group is a collection of clusters, where one of the clusters is a read/write primary and the others are read-only replicas. Writes to the primary are asynchronously propagated to the replicas.
--
-- A Redis (cluster mode enabled) replication group is a collection of 1 to 15 node groups (shards). Each node group (shard) has one read/write primary node and up to 5 read-only replica nodes. Writes to the primary are asynchronously propagated to the replicas. Redis (cluster mode enabled) replication groups partition the data across node groups (shards).
--
-- When a Redis (cluster mode disabled) replication group has been successfully created, you can add one or more read replicas to it, up to a total of 5 read replicas. You cannot alter a Redis (cluster mode enabled) replication group after it has been created. However, if you need to increase or decrease the number of node groups (console: shards), you can avail yourself of ElastiCache for Redis' enhanced backup and restore. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/backups-restoring.html Restoring From a Backup with Cluster Resizing> in the /ElastiCache User Guide/ .
--
module Network.AWS.ElastiCache.CreateReplicationGroup
    (
    -- * Creating a Request
      createReplicationGroup
    , CreateReplicationGroup
    -- * Request Lenses
    , crgAutomaticFailoverEnabled
    , crgEngineVersion
    , crgCacheNodeType
    , crgNodeGroupConfiguration
    , crgAtRestEncryptionEnabled
    , crgSecurityGroupIds
    , crgSnapshotARNs
    , crgAutoMinorVersionUpgrade
    , crgCacheParameterGroupName
    , crgTransitEncryptionEnabled
    , crgSnapshotWindow
    , crgAuthToken
    , crgPrimaryClusterId
    , crgEngine
    , crgPreferredMaintenanceWindow
    , crgCacheSubnetGroupName
    , crgNumNodeGroups
    , crgSnapshotRetentionLimit
    , crgReplicasPerNodeGroup
    , crgNumCacheClusters
    , crgPreferredCacheClusterAZs
    , crgSnapshotName
    , crgNotificationTopicARN
    , crgTags
    , crgPort
    , crgCacheSecurityGroupNames
    , crgReplicationGroupId
    , crgReplicationGroupDescription

    -- * Destructuring the Response
    , createReplicationGroupResponse
    , CreateReplicationGroupResponse
    -- * Response Lenses
    , crgrsReplicationGroup
    , crgrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @CreateReplicationGroup@ operation.
--
--
--
-- /See:/ 'createReplicationGroup' smart constructor.
data CreateReplicationGroup = CreateReplicationGroup'
  { _crgAutomaticFailoverEnabled    :: !(Maybe Bool)
  , _crgEngineVersion               :: !(Maybe Text)
  , _crgCacheNodeType               :: !(Maybe Text)
  , _crgNodeGroupConfiguration      :: !(Maybe [NodeGroupConfiguration])
  , _crgAtRestEncryptionEnabled     :: !(Maybe Bool)
  , _crgSecurityGroupIds            :: !(Maybe [Text])
  , _crgSnapshotARNs                :: !(Maybe [Text])
  , _crgAutoMinorVersionUpgrade     :: !(Maybe Bool)
  , _crgCacheParameterGroupName     :: !(Maybe Text)
  , _crgTransitEncryptionEnabled    :: !(Maybe Bool)
  , _crgSnapshotWindow              :: !(Maybe Text)
  , _crgAuthToken                   :: !(Maybe Text)
  , _crgPrimaryClusterId            :: !(Maybe Text)
  , _crgEngine                      :: !(Maybe Text)
  , _crgPreferredMaintenanceWindow  :: !(Maybe Text)
  , _crgCacheSubnetGroupName        :: !(Maybe Text)
  , _crgNumNodeGroups               :: !(Maybe Int)
  , _crgSnapshotRetentionLimit      :: !(Maybe Int)
  , _crgReplicasPerNodeGroup        :: !(Maybe Int)
  , _crgNumCacheClusters            :: !(Maybe Int)
  , _crgPreferredCacheClusterAZs    :: !(Maybe [Text])
  , _crgSnapshotName                :: !(Maybe Text)
  , _crgNotificationTopicARN        :: !(Maybe Text)
  , _crgTags                        :: !(Maybe [Tag])
  , _crgPort                        :: !(Maybe Int)
  , _crgCacheSecurityGroupNames     :: !(Maybe [Text])
  , _crgReplicationGroupId          :: !Text
  , _crgReplicationGroupDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgAutomaticFailoverEnabled' - Specifies whether a read-only replica is automatically promoted to read/write primary if the existing primary fails. If @true@ , Multi-AZ is enabled for this replication group. If @false@ , Multi-AZ is disabled for this replication group. @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode enabled) replication groups. Default: false Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
--
-- * 'crgEngineVersion' - The version number of the cache engine to be used for the clusters in this replication group. To view the supported cache engine versions, use the @DescribeCacheEngineVersions@ operation. __Important:__ You can upgrade to a newer engine version (see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ) in the /ElastiCache User Guide/ , but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
--
-- * 'crgCacheNodeType' - The compute and memory capacity of the nodes in the node group (shard). The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
-- * 'crgNodeGroupConfiguration' - A list of node group (shard) configuration options. Each node group (shard) configuration has the following: Slots, PrimaryAvailabilityZone, ReplicaAvailabilityZones, ReplicaCount. If you're creating a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group, you can use this parameter to individually configure each node group (shard), or you can omit this parameter.
--
-- * 'crgAtRestEncryptionEnabled' - A flag that enables encryption at rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.  Default: @false@
--
-- * 'crgSecurityGroupIds' - One or more Amazon VPC security groups associated with this replication group. Use this parameter only when you are creating a replication group in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- * 'crgSnapshotARNs' - A list of Amazon Resource Names (ARN) that uniquely identify the Redis RDB snapshot files stored in Amazon S3. The snapshot files are used to populate the new replication group. The Amazon S3 object name in the ARN cannot contain any commas. The new replication group will have the number of node groups (console: shards) specified by the parameter /NumNodeGroups/ or the number of node groups configured by /NodeGroupConfiguration/ regardless of the number of ARNs specified here. Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
--
-- * 'crgAutoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- * 'crgCacheParameterGroupName' - The name of the parameter group to associate with this replication group. If this argument is omitted, the default cache parameter group for the specified engine is used. If you are running Redis version 3.2.4 or later, only one node group (shard), and want to use a default parameter group, we recommend that you specify the parameter group by name.      * To create a Redis (cluster mode disabled) replication group, use @CacheParameterGroupName=default.redis3.2@ .     * To create a Redis (cluster mode enabled) replication group, use @CacheParameterGroupName=default.redis3.2.cluster.on@ .
--
-- * 'crgTransitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. This parameter is valid only if the @Engine@ parameter is @redis@ , the @EngineVersion@ parameter is @3.2.4@ or later, and the cluster is being created in an Amazon VPC. If you enable in-transit encryption, you must also specify a value for @CacheSubnetGroup@ . Default: @false@
--
-- * 'crgSnapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- * 'crgAuthToken' - __Reserved parameter.__ The password used to access a password protected server. This parameter is valid only if:     * The parameter @TransitEncryptionEnabled@ was set to @true@ when the cluster was created.     * The line @requirepass@ was added to the database configuration file. Password constraints:     * Must be only printable ASCII characters.     * Must be at least 16 characters and no more than 128 characters in length.     * Cannot contain any of the following characters: '/', '"', or '@'.  For more information, see <http://redis.io/commands/AUTH AUTH password> at http://redis.io/commands/AUTH.
--
-- * 'crgPrimaryClusterId' - The identifier of the cluster that serves as the primary for this replication group. This cluster must already exist and have a status of @available@ . This parameter is not required if @NumCacheClusters@ , @NumNodeGroups@ , or @ReplicasPerNodeGroup@ is specified.
--
-- * 'crgEngine' - The name of the cache engine to be used for the clusters in this replication group.
--
-- * 'crgPreferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are: Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
--
-- * 'crgCacheSubnetGroupName' - The name of the cache subnet group to be used for the replication group. /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SubnetGroups.html Subnets and Subnet Groups> .
--
-- * 'crgNumNodeGroups' - An optional parameter that specifies the number of node groups (shards) for this Redis (cluster mode enabled) replication group. For Redis (cluster mode disabled) either omit this parameter or set it to 1. Default: 1
--
-- * 'crgSnapshotRetentionLimit' - The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. Default: 0 (i.e., automatic backups are disabled for this cluster).
--
-- * 'crgReplicasPerNodeGroup' - An optional parameter that specifies the number of replica nodes in each node group (shard). Valid values are 0 to 5.
--
-- * 'crgNumCacheClusters' - The number of clusters this replication group initially has. This parameter is not used if there is more than one node group (shard). You should use @ReplicasPerNodeGroup@ instead. If @AutomaticFailoverEnabled@ is @true@ , the value of this parameter must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can omit this parameter (it will default to 1), or you can explicitly set it to a value between 2 and 6. The maximum permitted value for @NumCacheClusters@ is 6 (primary plus 5 replicas).
--
-- * 'crgPreferredCacheClusterAZs' - A list of EC2 Availability Zones in which the replication group's clusters are created. The order of the Availability Zones in the list is the order in which clusters are allocated. The primary cluster is created in the first AZ in the list. This parameter is not used if there is more than one node group (shard). You should use @NodeGroupConfiguration@ instead. Default: system chosen Availability Zones.
--
-- * 'crgSnapshotName' - The name of a snapshot from which to restore data into the new replication group. The snapshot status changes to @restoring@ while the new replication group is being created.
--
-- * 'crgNotificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
--
-- * 'crgTags' - A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key does not have to be accompanied by a tag value.
--
-- * 'crgPort' - The port number on which each member of the replication group accepts connections.
--
-- * 'crgCacheSecurityGroupNames' - A list of cache security group names to associate with this replication group.
--
-- * 'crgReplicationGroupId' - The replication group identifier. This parameter is stored as a lowercase string. Constraints:     * A name must contain from 1 to 20 alphanumeric characters or hyphens.     * The first character must be a letter.     * A name cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'crgReplicationGroupDescription' - A user-created description for the replication group.
createReplicationGroup
    :: Text -- ^ 'crgReplicationGroupId'
    -> Text -- ^ 'crgReplicationGroupDescription'
    -> CreateReplicationGroup
createReplicationGroup pReplicationGroupId_ pReplicationGroupDescription_ =
  CreateReplicationGroup'
    { _crgAutomaticFailoverEnabled = Nothing
    , _crgEngineVersion = Nothing
    , _crgCacheNodeType = Nothing
    , _crgNodeGroupConfiguration = Nothing
    , _crgAtRestEncryptionEnabled = Nothing
    , _crgSecurityGroupIds = Nothing
    , _crgSnapshotARNs = Nothing
    , _crgAutoMinorVersionUpgrade = Nothing
    , _crgCacheParameterGroupName = Nothing
    , _crgTransitEncryptionEnabled = Nothing
    , _crgSnapshotWindow = Nothing
    , _crgAuthToken = Nothing
    , _crgPrimaryClusterId = Nothing
    , _crgEngine = Nothing
    , _crgPreferredMaintenanceWindow = Nothing
    , _crgCacheSubnetGroupName = Nothing
    , _crgNumNodeGroups = Nothing
    , _crgSnapshotRetentionLimit = Nothing
    , _crgReplicasPerNodeGroup = Nothing
    , _crgNumCacheClusters = Nothing
    , _crgPreferredCacheClusterAZs = Nothing
    , _crgSnapshotName = Nothing
    , _crgNotificationTopicARN = Nothing
    , _crgTags = Nothing
    , _crgPort = Nothing
    , _crgCacheSecurityGroupNames = Nothing
    , _crgReplicationGroupId = pReplicationGroupId_
    , _crgReplicationGroupDescription = pReplicationGroupDescription_
    }


-- | Specifies whether a read-only replica is automatically promoted to read/write primary if the existing primary fails. If @true@ , Multi-AZ is enabled for this replication group. If @false@ , Multi-AZ is disabled for this replication group. @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode enabled) replication groups. Default: false Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
crgAutomaticFailoverEnabled :: Lens' CreateReplicationGroup (Maybe Bool)
crgAutomaticFailoverEnabled = lens _crgAutomaticFailoverEnabled (\ s a -> s{_crgAutomaticFailoverEnabled = a})

-- | The version number of the cache engine to be used for the clusters in this replication group. To view the supported cache engine versions, use the @DescribeCacheEngineVersions@ operation. __Important:__ You can upgrade to a newer engine version (see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ) in the /ElastiCache User Guide/ , but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
crgEngineVersion :: Lens' CreateReplicationGroup (Maybe Text)
crgEngineVersion = lens _crgEngineVersion (\ s a -> s{_crgEngineVersion = a})

-- | The compute and memory capacity of the nodes in the node group (shard). The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
crgCacheNodeType :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheNodeType = lens _crgCacheNodeType (\ s a -> s{_crgCacheNodeType = a})

-- | A list of node group (shard) configuration options. Each node group (shard) configuration has the following: Slots, PrimaryAvailabilityZone, ReplicaAvailabilityZones, ReplicaCount. If you're creating a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group, you can use this parameter to individually configure each node group (shard), or you can omit this parameter.
crgNodeGroupConfiguration :: Lens' CreateReplicationGroup [NodeGroupConfiguration]
crgNodeGroupConfiguration = lens _crgNodeGroupConfiguration (\ s a -> s{_crgNodeGroupConfiguration = a}) . _Default . _Coerce

-- | A flag that enables encryption at rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.  Default: @false@
crgAtRestEncryptionEnabled :: Lens' CreateReplicationGroup (Maybe Bool)
crgAtRestEncryptionEnabled = lens _crgAtRestEncryptionEnabled (\ s a -> s{_crgAtRestEncryptionEnabled = a})

-- | One or more Amazon VPC security groups associated with this replication group. Use this parameter only when you are creating a replication group in an Amazon Virtual Private Cloud (Amazon VPC).
crgSecurityGroupIds :: Lens' CreateReplicationGroup [Text]
crgSecurityGroupIds = lens _crgSecurityGroupIds (\ s a -> s{_crgSecurityGroupIds = a}) . _Default . _Coerce

-- | A list of Amazon Resource Names (ARN) that uniquely identify the Redis RDB snapshot files stored in Amazon S3. The snapshot files are used to populate the new replication group. The Amazon S3 object name in the ARN cannot contain any commas. The new replication group will have the number of node groups (console: shards) specified by the parameter /NumNodeGroups/ or the number of node groups configured by /NodeGroupConfiguration/ regardless of the number of ARNs specified here. Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
crgSnapshotARNs :: Lens' CreateReplicationGroup [Text]
crgSnapshotARNs = lens _crgSnapshotARNs (\ s a -> s{_crgSnapshotARNs = a}) . _Default . _Coerce

-- | This parameter is currently disabled.
crgAutoMinorVersionUpgrade :: Lens' CreateReplicationGroup (Maybe Bool)
crgAutoMinorVersionUpgrade = lens _crgAutoMinorVersionUpgrade (\ s a -> s{_crgAutoMinorVersionUpgrade = a})

-- | The name of the parameter group to associate with this replication group. If this argument is omitted, the default cache parameter group for the specified engine is used. If you are running Redis version 3.2.4 or later, only one node group (shard), and want to use a default parameter group, we recommend that you specify the parameter group by name.      * To create a Redis (cluster mode disabled) replication group, use @CacheParameterGroupName=default.redis3.2@ .     * To create a Redis (cluster mode enabled) replication group, use @CacheParameterGroupName=default.redis3.2.cluster.on@ .
crgCacheParameterGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheParameterGroupName = lens _crgCacheParameterGroupName (\ s a -> s{_crgCacheParameterGroupName = a})

-- | A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. This parameter is valid only if the @Engine@ parameter is @redis@ , the @EngineVersion@ parameter is @3.2.4@ or later, and the cluster is being created in an Amazon VPC. If you enable in-transit encryption, you must also specify a value for @CacheSubnetGroup@ . Default: @false@
crgTransitEncryptionEnabled :: Lens' CreateReplicationGroup (Maybe Bool)
crgTransitEncryptionEnabled = lens _crgTransitEncryptionEnabled (\ s a -> s{_crgTransitEncryptionEnabled = a})

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
crgSnapshotWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgSnapshotWindow = lens _crgSnapshotWindow (\ s a -> s{_crgSnapshotWindow = a})

-- | __Reserved parameter.__ The password used to access a password protected server. This parameter is valid only if:     * The parameter @TransitEncryptionEnabled@ was set to @true@ when the cluster was created.     * The line @requirepass@ was added to the database configuration file. Password constraints:     * Must be only printable ASCII characters.     * Must be at least 16 characters and no more than 128 characters in length.     * Cannot contain any of the following characters: '/', '"', or '@'.  For more information, see <http://redis.io/commands/AUTH AUTH password> at http://redis.io/commands/AUTH.
crgAuthToken :: Lens' CreateReplicationGroup (Maybe Text)
crgAuthToken = lens _crgAuthToken (\ s a -> s{_crgAuthToken = a})

-- | The identifier of the cluster that serves as the primary for this replication group. This cluster must already exist and have a status of @available@ . This parameter is not required if @NumCacheClusters@ , @NumNodeGroups@ , or @ReplicasPerNodeGroup@ is specified.
crgPrimaryClusterId :: Lens' CreateReplicationGroup (Maybe Text)
crgPrimaryClusterId = lens _crgPrimaryClusterId (\ s a -> s{_crgPrimaryClusterId = a})

-- | The name of the cache engine to be used for the clusters in this replication group.
crgEngine :: Lens' CreateReplicationGroup (Maybe Text)
crgEngine = lens _crgEngine (\ s a -> s{_crgEngine = a})

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are: Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
crgPreferredMaintenanceWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgPreferredMaintenanceWindow = lens _crgPreferredMaintenanceWindow (\ s a -> s{_crgPreferredMaintenanceWindow = a})

-- | The name of the cache subnet group to be used for the replication group. /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SubnetGroups.html Subnets and Subnet Groups> .
crgCacheSubnetGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheSubnetGroupName = lens _crgCacheSubnetGroupName (\ s a -> s{_crgCacheSubnetGroupName = a})

-- | An optional parameter that specifies the number of node groups (shards) for this Redis (cluster mode enabled) replication group. For Redis (cluster mode disabled) either omit this parameter or set it to 1. Default: 1
crgNumNodeGroups :: Lens' CreateReplicationGroup (Maybe Int)
crgNumNodeGroups = lens _crgNumNodeGroups (\ s a -> s{_crgNumNodeGroups = a})

-- | The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. Default: 0 (i.e., automatic backups are disabled for this cluster).
crgSnapshotRetentionLimit :: Lens' CreateReplicationGroup (Maybe Int)
crgSnapshotRetentionLimit = lens _crgSnapshotRetentionLimit (\ s a -> s{_crgSnapshotRetentionLimit = a})

-- | An optional parameter that specifies the number of replica nodes in each node group (shard). Valid values are 0 to 5.
crgReplicasPerNodeGroup :: Lens' CreateReplicationGroup (Maybe Int)
crgReplicasPerNodeGroup = lens _crgReplicasPerNodeGroup (\ s a -> s{_crgReplicasPerNodeGroup = a})

-- | The number of clusters this replication group initially has. This parameter is not used if there is more than one node group (shard). You should use @ReplicasPerNodeGroup@ instead. If @AutomaticFailoverEnabled@ is @true@ , the value of this parameter must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can omit this parameter (it will default to 1), or you can explicitly set it to a value between 2 and 6. The maximum permitted value for @NumCacheClusters@ is 6 (primary plus 5 replicas).
crgNumCacheClusters :: Lens' CreateReplicationGroup (Maybe Int)
crgNumCacheClusters = lens _crgNumCacheClusters (\ s a -> s{_crgNumCacheClusters = a})

-- | A list of EC2 Availability Zones in which the replication group's clusters are created. The order of the Availability Zones in the list is the order in which clusters are allocated. The primary cluster is created in the first AZ in the list. This parameter is not used if there is more than one node group (shard). You should use @NodeGroupConfiguration@ instead. Default: system chosen Availability Zones.
crgPreferredCacheClusterAZs :: Lens' CreateReplicationGroup [Text]
crgPreferredCacheClusterAZs = lens _crgPreferredCacheClusterAZs (\ s a -> s{_crgPreferredCacheClusterAZs = a}) . _Default . _Coerce

-- | The name of a snapshot from which to restore data into the new replication group. The snapshot status changes to @restoring@ while the new replication group is being created.
crgSnapshotName :: Lens' CreateReplicationGroup (Maybe Text)
crgSnapshotName = lens _crgSnapshotName (\ s a -> s{_crgSnapshotName = a})

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
crgNotificationTopicARN :: Lens' CreateReplicationGroup (Maybe Text)
crgNotificationTopicARN = lens _crgNotificationTopicARN (\ s a -> s{_crgNotificationTopicARN = a})

-- | A list of cost allocation tags to be added to this resource. A tag is a key-value pair. A tag key does not have to be accompanied by a tag value.
crgTags :: Lens' CreateReplicationGroup [Tag]
crgTags = lens _crgTags (\ s a -> s{_crgTags = a}) . _Default . _Coerce

-- | The port number on which each member of the replication group accepts connections.
crgPort :: Lens' CreateReplicationGroup (Maybe Int)
crgPort = lens _crgPort (\ s a -> s{_crgPort = a})

-- | A list of cache security group names to associate with this replication group.
crgCacheSecurityGroupNames :: Lens' CreateReplicationGroup [Text]
crgCacheSecurityGroupNames = lens _crgCacheSecurityGroupNames (\ s a -> s{_crgCacheSecurityGroupNames = a}) . _Default . _Coerce

-- | The replication group identifier. This parameter is stored as a lowercase string. Constraints:     * A name must contain from 1 to 20 alphanumeric characters or hyphens.     * The first character must be a letter.     * A name cannot end with a hyphen or contain two consecutive hyphens.
crgReplicationGroupId :: Lens' CreateReplicationGroup Text
crgReplicationGroupId = lens _crgReplicationGroupId (\ s a -> s{_crgReplicationGroupId = a})

-- | A user-created description for the replication group.
crgReplicationGroupDescription :: Lens' CreateReplicationGroup Text
crgReplicationGroupDescription = lens _crgReplicationGroupDescription (\ s a -> s{_crgReplicationGroupDescription = a})

instance AWSRequest CreateReplicationGroup where
        type Rs CreateReplicationGroup =
             CreateReplicationGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "CreateReplicationGroupResult"
              (\ s h x ->
                 CreateReplicationGroupResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance Hashable CreateReplicationGroup where

instance NFData CreateReplicationGroup where

instance ToHeaders CreateReplicationGroup where
        toHeaders = const mempty

instance ToPath CreateReplicationGroup where
        toPath = const "/"

instance ToQuery CreateReplicationGroup where
        toQuery CreateReplicationGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateReplicationGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "AutomaticFailoverEnabled" =:
                 _crgAutomaticFailoverEnabled,
               "EngineVersion" =: _crgEngineVersion,
               "CacheNodeType" =: _crgCacheNodeType,
               "NodeGroupConfiguration" =:
                 toQuery
                   (toQueryList "NodeGroupConfiguration" <$>
                      _crgNodeGroupConfiguration),
               "AtRestEncryptionEnabled" =:
                 _crgAtRestEncryptionEnabled,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _crgSecurityGroupIds),
               "SnapshotArns" =:
                 toQuery
                   (toQueryList "SnapshotArn" <$> _crgSnapshotARNs),
               "AutoMinorVersionUpgrade" =:
                 _crgAutoMinorVersionUpgrade,
               "CacheParameterGroupName" =:
                 _crgCacheParameterGroupName,
               "TransitEncryptionEnabled" =:
                 _crgTransitEncryptionEnabled,
               "SnapshotWindow" =: _crgSnapshotWindow,
               "AuthToken" =: _crgAuthToken,
               "PrimaryClusterId" =: _crgPrimaryClusterId,
               "Engine" =: _crgEngine,
               "PreferredMaintenanceWindow" =:
                 _crgPreferredMaintenanceWindow,
               "CacheSubnetGroupName" =: _crgCacheSubnetGroupName,
               "NumNodeGroups" =: _crgNumNodeGroups,
               "SnapshotRetentionLimit" =:
                 _crgSnapshotRetentionLimit,
               "ReplicasPerNodeGroup" =: _crgReplicasPerNodeGroup,
               "NumCacheClusters" =: _crgNumCacheClusters,
               "PreferredCacheClusterAZs" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _crgPreferredCacheClusterAZs),
               "SnapshotName" =: _crgSnapshotName,
               "NotificationTopicArn" =: _crgNotificationTopicARN,
               "Tags" =: toQuery (toQueryList "Tag" <$> _crgTags),
               "Port" =: _crgPort,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _crgCacheSecurityGroupNames),
               "ReplicationGroupId" =: _crgReplicationGroupId,
               "ReplicationGroupDescription" =:
                 _crgReplicationGroupDescription]

-- | /See:/ 'createReplicationGroupResponse' smart constructor.
data CreateReplicationGroupResponse = CreateReplicationGroupResponse'
  { _crgrsReplicationGroup :: !(Maybe ReplicationGroup)
  , _crgrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crgrsReplicationGroup' - Undocumented member.
--
-- * 'crgrsResponseStatus' - -- | The response status code.
createReplicationGroupResponse
    :: Int -- ^ 'crgrsResponseStatus'
    -> CreateReplicationGroupResponse
createReplicationGroupResponse pResponseStatus_ =
  CreateReplicationGroupResponse'
    {_crgrsReplicationGroup = Nothing, _crgrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
crgrsReplicationGroup :: Lens' CreateReplicationGroupResponse (Maybe ReplicationGroup)
crgrsReplicationGroup = lens _crgrsReplicationGroup (\ s a -> s{_crgrsReplicationGroup = a})

-- | -- | The response status code.
crgrsResponseStatus :: Lens' CreateReplicationGroupResponse Int
crgrsResponseStatus = lens _crgrsResponseStatus (\ s a -> s{_crgrsResponseStatus = a})

instance NFData CreateReplicationGroupResponse where
