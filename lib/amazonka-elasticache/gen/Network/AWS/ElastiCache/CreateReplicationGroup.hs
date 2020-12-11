{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group.
--
-- This API can be used to create a standalone regional replication group or a secondary replication group associated with a Global Datastore.
-- A Redis (cluster mode disabled) replication group is a collection of clusters, where one of the clusters is a read/write primary and the others are read-only replicas. Writes to the primary are asynchronously propagated to the replicas.
-- A Redis (cluster mode enabled) replication group is a collection of 1 to 90 node groups (shards). Each node group (shard) has one read/write primary node and up to 5 read-only replica nodes. Writes to the primary are asynchronously propagated to the replicas. Redis (cluster mode enabled) replication groups partition the data across node groups (shards).
-- When a Redis (cluster mode disabled) replication group has been successfully created, you can add one or more read replicas to it, up to a total of 5 read replicas. If you need to increase or decrease the number of node groups (console: shards), you can avail yourself of ElastiCache for Redis' scaling. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Scaling.html Scaling ElastiCache for Redis Clusters> in the /ElastiCache User Guide/ .
module Network.AWS.ElastiCache.CreateReplicationGroup
  ( -- * Creating a request
    CreateReplicationGroup (..),
    mkCreateReplicationGroup,

    -- ** Request lenses
    crgAutomaticFailoverEnabled,
    crgEngineVersion,
    crgCacheNodeType,
    crgNodeGroupConfiguration,
    crgAtRestEncryptionEnabled,
    crgSecurityGroupIds,
    crgSnapshotARNs,
    crgAutoMinorVersionUpgrade,
    crgCacheParameterGroupName,
    crgTransitEncryptionEnabled,
    crgUserGroupIds,
    crgSnapshotWindow,
    crgAuthToken,
    crgPrimaryClusterId,
    crgEngine,
    crgPreferredMaintenanceWindow,
    crgKMSKeyId,
    crgMultiAZEnabled,
    crgCacheSubnetGroupName,
    crgNumNodeGroups,
    crgSnapshotRetentionLimit,
    crgGlobalReplicationGroupId,
    crgReplicasPerNodeGroup,
    crgNumCacheClusters,
    crgPreferredCacheClusterAZs,
    crgSnapshotName,
    crgNotificationTopicARN,
    crgTags,
    crgPort,
    crgCacheSecurityGroupNames,
    crgReplicationGroupId,
    crgReplicationGroupDescription,

    -- * Destructuring the response
    CreateReplicationGroupResponse (..),
    mkCreateReplicationGroupResponse,

    -- ** Response lenses
    crgrsReplicationGroup,
    crgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @CreateReplicationGroup@ operation.
--
-- /See:/ 'mkCreateReplicationGroup' smart constructor.
data CreateReplicationGroup = CreateReplicationGroup'
  { automaticFailoverEnabled ::
      Lude.Maybe Lude.Bool,
    engineVersion :: Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    nodeGroupConfiguration ::
      Lude.Maybe [NodeGroupConfiguration],
    atRestEncryptionEnabled ::
      Lude.Maybe Lude.Bool,
    securityGroupIds :: Lude.Maybe [Lude.Text],
    snapshotARNs :: Lude.Maybe [Lude.Text],
    autoMinorVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    cacheParameterGroupName ::
      Lude.Maybe Lude.Text,
    transitEncryptionEnabled ::
      Lude.Maybe Lude.Bool,
    userGroupIds ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    snapshotWindow :: Lude.Maybe Lude.Text,
    authToken :: Lude.Maybe Lude.Text,
    primaryClusterId :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow ::
      Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    multiAZEnabled :: Lude.Maybe Lude.Bool,
    cacheSubnetGroupName :: Lude.Maybe Lude.Text,
    numNodeGroups :: Lude.Maybe Lude.Int,
    snapshotRetentionLimit :: Lude.Maybe Lude.Int,
    globalReplicationGroupId ::
      Lude.Maybe Lude.Text,
    replicasPerNodeGroup :: Lude.Maybe Lude.Int,
    numCacheClusters :: Lude.Maybe Lude.Int,
    preferredCacheClusterAZs ::
      Lude.Maybe [Lude.Text],
    snapshotName :: Lude.Maybe Lude.Text,
    notificationTopicARN :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    cacheSecurityGroupNames ::
      Lude.Maybe [Lude.Text],
    replicationGroupId :: Lude.Text,
    replicationGroupDescription :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationGroup' with the minimum fields required to make a request.
--
-- * 'atRestEncryptionEnabled' - A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
-- * 'authToken' - __Reserved parameter.__ The password used to access a password protected server.
--
-- @AuthToken@ can be specified only on replication groups where @TransitEncryptionEnabled@ is @true@ .
-- /Important:/ For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as @true@ , an @AuthToken@ , and a @CacheSubnetGroup@ .
-- Password constraints:
--
--     * Must be only printable ASCII characters.
--
--
--     * Must be at least 16 characters and no more than 128 characters in length.
--
--
--     * The only permitted printable special characters are !, &, #, $, ^, <, >, and -. Other printable special characters cannot be used in the AUTH token.
--
--
-- For more information, see <http://redis.io/commands/AUTH AUTH password> at http://redis.io/commands/AUTH.
-- * 'autoMinorVersionUpgrade' - This parameter is currently disabled.
-- * 'automaticFailoverEnabled' - Specifies whether a read-only replica is automatically promoted to read/write primary if the existing primary fails.
--
-- @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode enabled) replication groups.
-- Default: false
-- * 'cacheNodeType' - The compute and memory capacity of the nodes in the node group (shard).
--
-- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
--
--     * General purpose:
--
--     * Current generation:
-- __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@
-- __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@
-- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
-- __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@
-- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
--
--
--     * Previous generation: (not recommended)
-- __T1 node types:__ @cache.t1.micro@
-- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
-- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
--
--
--
--
--     * Compute optimized:
--
--     * Previous generation: (not recommended)
-- __C1 node types:__ @cache.c1.xlarge@
--
--
--
--
--     * Memory optimized:
--
--     * Current generation:
-- __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@
-- __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@
-- __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@
--
--
--     * Previous generation: (not recommended)
-- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
-- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
--
--
--
--
-- __Additional node type info__
--
--     * All current generation instance types are created in Amazon VPC by default.
--
--
--     * Redis append-only files (AOF) are not supported for T1 or T2 instances.
--
--
--     * Redis Multi-AZ with automatic failover is not supported on T1 instances.
--
--
--     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
--
-- * 'cacheParameterGroupName' - The name of the parameter group to associate with this replication group. If this argument is omitted, the default cache parameter group for the specified engine is used.
--
-- If you are running Redis version 3.2.4 or later, only one node group (shard), and want to use a default parameter group, we recommend that you specify the parameter group by name.
--
--     * To create a Redis (cluster mode disabled) replication group, use @CacheParameterGroupName=default.redis3.2@ .
--
--
--     * To create a Redis (cluster mode enabled) replication group, use @CacheParameterGroupName=default.redis3.2.cluster.on@ .
--
--
-- * 'cacheSecurityGroupNames' - A list of cache security group names to associate with this replication group.
-- * 'cacheSubnetGroupName' - The name of the cache subnet group to be used for the replication group.
--
-- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
-- * 'engine' - The name of the cache engine to be used for the clusters in this replication group.
-- * 'engineVersion' - The version number of the cache engine to be used for the clusters in this replication group. To view the supported cache engine versions, use the @DescribeCacheEngineVersions@ operation.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ) in the /ElastiCache User Guide/ , but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
-- * 'globalReplicationGroupId' - The name of the Global Datastore
-- * 'kmsKeyId' - The ID of the KMS key used to encrypt the disk in the cluster.
-- * 'multiAZEnabled' - A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
-- * 'nodeGroupConfiguration' - A list of node group (shard) configuration options. Each node group (shard) configuration has the following members: @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ , and @Slots@ .
--
-- If you're creating a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group, you can use this parameter to individually configure each node group (shard), or you can omit this parameter. However, it is required when seeding a Redis (cluster mode enabled) cluster from a S3 rdb file. You must configure each node group (shard) using this parameter because you must specify the slots for each node group.
-- * 'notificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
-- * 'numCacheClusters' - The number of clusters this replication group initially has.
--
-- This parameter is not used if there is more than one node group (shard). You should use @ReplicasPerNodeGroup@ instead.
-- If @AutomaticFailoverEnabled@ is @true@ , the value of this parameter must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can omit this parameter (it will default to 1), or you can explicitly set it to a value between 2 and 6.
-- The maximum permitted value for @NumCacheClusters@ is 6 (1 primary plus 5 replicas).
-- * 'numNodeGroups' - An optional parameter that specifies the number of node groups (shards) for this Redis (cluster mode enabled) replication group. For Redis (cluster mode disabled) either omit this parameter or set it to 1.
--
-- Default: 1
-- * 'port' - The port number on which each member of the replication group accepts connections.
-- * 'preferredCacheClusterAZs' - A list of EC2 Availability Zones in which the replication group's clusters are created. The order of the Availability Zones in the list is the order in which clusters are allocated. The primary cluster is created in the first AZ in the list.
--
-- This parameter is not used if there is more than one node group (shard). You should use @NodeGroupConfiguration@ instead.
-- Default: system chosen Availability Zones.
-- * 'preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:
--
-- Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
-- Valid values for @ddd@ are:
--
--     * @sun@
--
--
--     * @mon@
--
--
--     * @tue@
--
--
--     * @wed@
--
--
--     * @thu@
--
--
--     * @fri@
--
--
--     * @sat@
--
--
-- Example: @sun:23:00-mon:01:30@
-- * 'primaryClusterId' - The identifier of the cluster that serves as the primary for this replication group. This cluster must already exist and have a status of @available@ .
--
-- This parameter is not required if @NumCacheClusters@ , @NumNodeGroups@ , or @ReplicasPerNodeGroup@ is specified.
-- * 'replicasPerNodeGroup' - An optional parameter that specifies the number of replica nodes in each node group (shard). Valid values are 0 to 5.
-- * 'replicationGroupDescription' - A user-created description for the replication group.
-- * 'replicationGroupId' - The replication group identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * A name must contain from 1 to 40 alphanumeric characters or hyphens.
--
--
--     * The first character must be a letter.
--
--
--     * A name cannot end with a hyphen or contain two consecutive hyphens.
--
--
-- * 'securityGroupIds' - One or more Amazon VPC security groups associated with this replication group.
--
-- Use this parameter only when you are creating a replication group in an Amazon Virtual Private Cloud (Amazon VPC).
-- * 'snapshotARNs' - A list of Amazon Resource Names (ARN) that uniquely identify the Redis RDB snapshot files stored in Amazon S3. The snapshot files are used to populate the new replication group. The Amazon S3 object name in the ARN cannot contain any commas. The new replication group will have the number of node groups (console: shards) specified by the parameter /NumNodeGroups/ or the number of node groups configured by /NodeGroupConfiguration/ regardless of the number of ARNs specified here.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
-- * 'snapshotName' - The name of a snapshot from which to restore data into the new replication group. The snapshot status changes to @restoring@ while the new replication group is being created.
-- * 'snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cluster).
-- * 'snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
-- * 'tags' - A list of cost allocation tags to be added to this resource. Tags are comma-separated key,value pairs (e.g. Key=@myKey@ , Value=@myKeyValue@ . You can include multiple tags as shown following: Key=@myKey@ , Value=@myKeyValue@ Key=@mySecondKey@ , Value=@mySecondKeyValue@ .
-- * 'transitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- This parameter is valid only if the @Engine@ parameter is @redis@ , the @EngineVersion@ parameter is @3.2.6@ , @4.x@ or later, and the cluster is being created in an Amazon VPC.
-- If you enable in-transit encryption, you must also specify a value for @CacheSubnetGroup@ .
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
-- /Important:/ For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as @true@ , an @AuthToken@ , and a @CacheSubnetGroup@ .
-- * 'userGroupIds' - The list of user groups to associate with the replication group.
mkCreateReplicationGroup ::
  -- | 'replicationGroupId'
  Lude.Text ->
  -- | 'replicationGroupDescription'
  Lude.Text ->
  CreateReplicationGroup
mkCreateReplicationGroup
  pReplicationGroupId_
  pReplicationGroupDescription_ =
    CreateReplicationGroup'
      { automaticFailoverEnabled = Lude.Nothing,
        engineVersion = Lude.Nothing,
        cacheNodeType = Lude.Nothing,
        nodeGroupConfiguration = Lude.Nothing,
        atRestEncryptionEnabled = Lude.Nothing,
        securityGroupIds = Lude.Nothing,
        snapshotARNs = Lude.Nothing,
        autoMinorVersionUpgrade = Lude.Nothing,
        cacheParameterGroupName = Lude.Nothing,
        transitEncryptionEnabled = Lude.Nothing,
        userGroupIds = Lude.Nothing,
        snapshotWindow = Lude.Nothing,
        authToken = Lude.Nothing,
        primaryClusterId = Lude.Nothing,
        engine = Lude.Nothing,
        preferredMaintenanceWindow = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        multiAZEnabled = Lude.Nothing,
        cacheSubnetGroupName = Lude.Nothing,
        numNodeGroups = Lude.Nothing,
        snapshotRetentionLimit = Lude.Nothing,
        globalReplicationGroupId = Lude.Nothing,
        replicasPerNodeGroup = Lude.Nothing,
        numCacheClusters = Lude.Nothing,
        preferredCacheClusterAZs = Lude.Nothing,
        snapshotName = Lude.Nothing,
        notificationTopicARN = Lude.Nothing,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        cacheSecurityGroupNames = Lude.Nothing,
        replicationGroupId = pReplicationGroupId_,
        replicationGroupDescription = pReplicationGroupDescription_
      }

-- | Specifies whether a read-only replica is automatically promoted to read/write primary if the existing primary fails.
--
-- @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode enabled) replication groups.
-- Default: false
--
-- /Note:/ Consider using 'automaticFailoverEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgAutomaticFailoverEnabled :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Bool)
crgAutomaticFailoverEnabled = Lens.lens (automaticFailoverEnabled :: CreateReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {automaticFailoverEnabled = a} :: CreateReplicationGroup)
{-# DEPRECATED crgAutomaticFailoverEnabled "Use generic-lens or generic-optics with 'automaticFailoverEnabled' instead." #-}

-- | The version number of the cache engine to be used for the clusters in this replication group. To view the supported cache engine versions, use the @DescribeCacheEngineVersions@ operation.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ) in the /ElastiCache User Guide/ , but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgEngineVersion :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgEngineVersion = Lens.lens (engineVersion :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateReplicationGroup)
{-# DEPRECATED crgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The compute and memory capacity of the nodes in the node group (shard).
--
-- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
--
--     * General purpose:
--
--     * Current generation:
-- __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@
-- __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@
-- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
-- __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@
-- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
--
--
--     * Previous generation: (not recommended)
-- __T1 node types:__ @cache.t1.micro@
-- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
-- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
--
--
--
--
--     * Compute optimized:
--
--     * Previous generation: (not recommended)
-- __C1 node types:__ @cache.c1.xlarge@
--
--
--
--
--     * Memory optimized:
--
--     * Current generation:
-- __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@
-- __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@
-- __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@
--
--
--     * Previous generation: (not recommended)
-- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
-- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
--
--
--
--
-- __Additional node type info__
--
--     * All current generation instance types are created in Amazon VPC by default.
--
--
--     * Redis append-only files (AOF) are not supported for T1 or T2 instances.
--
--
--     * Redis Multi-AZ with automatic failover is not supported on T1 instances.
--
--
--     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
--
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgCacheNodeType :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgCacheNodeType = Lens.lens (cacheNodeType :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: CreateReplicationGroup)
{-# DEPRECATED crgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | A list of node group (shard) configuration options. Each node group (shard) configuration has the following members: @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ , and @Slots@ .
--
-- If you're creating a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group, you can use this parameter to individually configure each node group (shard), or you can omit this parameter. However, it is required when seeding a Redis (cluster mode enabled) cluster from a S3 rdb file. You must configure each node group (shard) using this parameter because you must specify the slots for each node group.
--
-- /Note:/ Consider using 'nodeGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNodeGroupConfiguration :: Lens.Lens' CreateReplicationGroup (Lude.Maybe [NodeGroupConfiguration])
crgNodeGroupConfiguration = Lens.lens (nodeGroupConfiguration :: CreateReplicationGroup -> Lude.Maybe [NodeGroupConfiguration]) (\s a -> s {nodeGroupConfiguration = a} :: CreateReplicationGroup)
{-# DEPRECATED crgNodeGroupConfiguration "Use generic-lens or generic-optics with 'nodeGroupConfiguration' instead." #-}

-- | A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgAtRestEncryptionEnabled :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Bool)
crgAtRestEncryptionEnabled = Lens.lens (atRestEncryptionEnabled :: CreateReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {atRestEncryptionEnabled = a} :: CreateReplicationGroup)
{-# DEPRECATED crgAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | One or more Amazon VPC security groups associated with this replication group.
--
-- Use this parameter only when you are creating a replication group in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSecurityGroupIds :: Lens.Lens' CreateReplicationGroup (Lude.Maybe [Lude.Text])
crgSecurityGroupIds = Lens.lens (securityGroupIds :: CreateReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateReplicationGroup)
{-# DEPRECATED crgSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A list of Amazon Resource Names (ARN) that uniquely identify the Redis RDB snapshot files stored in Amazon S3. The snapshot files are used to populate the new replication group. The Amazon S3 object name in the ARN cannot contain any commas. The new replication group will have the number of node groups (console: shards) specified by the parameter /NumNodeGroups/ or the number of node groups configured by /NodeGroupConfiguration/ regardless of the number of ARNs specified here.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
--
-- /Note:/ Consider using 'snapshotARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotARNs :: Lens.Lens' CreateReplicationGroup (Lude.Maybe [Lude.Text])
crgSnapshotARNs = Lens.lens (snapshotARNs :: CreateReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {snapshotARNs = a} :: CreateReplicationGroup)
{-# DEPRECATED crgSnapshotARNs "Use generic-lens or generic-optics with 'snapshotARNs' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgAutoMinorVersionUpgrade :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Bool)
crgAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: CreateReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: CreateReplicationGroup)
{-# DEPRECATED crgAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The name of the parameter group to associate with this replication group. If this argument is omitted, the default cache parameter group for the specified engine is used.
--
-- If you are running Redis version 3.2.4 or later, only one node group (shard), and want to use a default parameter group, we recommend that you specify the parameter group by name.
--
--     * To create a Redis (cluster mode disabled) replication group, use @CacheParameterGroupName=default.redis3.2@ .
--
--
--     * To create a Redis (cluster mode enabled) replication group, use @CacheParameterGroupName=default.redis3.2.cluster.on@ .
--
--
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgCacheParameterGroupName :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: CreateReplicationGroup)
{-# DEPRECATED crgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- This parameter is valid only if the @Engine@ parameter is @redis@ , the @EngineVersion@ parameter is @3.2.6@ , @4.x@ or later, and the cluster is being created in an Amazon VPC.
-- If you enable in-transit encryption, you must also specify a value for @CacheSubnetGroup@ .
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
-- /Important:/ For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as @true@ , an @AuthToken@ , and a @CacheSubnetGroup@ .
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgTransitEncryptionEnabled :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Bool)
crgTransitEncryptionEnabled = Lens.lens (transitEncryptionEnabled :: CreateReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {transitEncryptionEnabled = a} :: CreateReplicationGroup)
{-# DEPRECATED crgTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

-- | The list of user groups to associate with the replication group.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgUserGroupIds :: Lens.Lens' CreateReplicationGroup (Lude.Maybe (Lude.NonEmpty Lude.Text))
crgUserGroupIds = Lens.lens (userGroupIds :: CreateReplicationGroup -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {userGroupIds = a} :: CreateReplicationGroup)
{-# DEPRECATED crgUserGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotWindow :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgSnapshotWindow = Lens.lens (snapshotWindow :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {snapshotWindow = a} :: CreateReplicationGroup)
{-# DEPRECATED crgSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | __Reserved parameter.__ The password used to access a password protected server.
--
-- @AuthToken@ can be specified only on replication groups where @TransitEncryptionEnabled@ is @true@ .
-- /Important:/ For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as @true@ , an @AuthToken@ , and a @CacheSubnetGroup@ .
-- Password constraints:
--
--     * Must be only printable ASCII characters.
--
--
--     * Must be at least 16 characters and no more than 128 characters in length.
--
--
--     * The only permitted printable special characters are !, &, #, $, ^, <, >, and -. Other printable special characters cannot be used in the AUTH token.
--
--
-- For more information, see <http://redis.io/commands/AUTH AUTH password> at http://redis.io/commands/AUTH.
--
-- /Note:/ Consider using 'authToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgAuthToken :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgAuthToken = Lens.lens (authToken :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {authToken = a} :: CreateReplicationGroup)
{-# DEPRECATED crgAuthToken "Use generic-lens or generic-optics with 'authToken' instead." #-}

-- | The identifier of the cluster that serves as the primary for this replication group. This cluster must already exist and have a status of @available@ .
--
-- This parameter is not required if @NumCacheClusters@ , @NumNodeGroups@ , or @ReplicasPerNodeGroup@ is specified.
--
-- /Note:/ Consider using 'primaryClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgPrimaryClusterId :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgPrimaryClusterId = Lens.lens (primaryClusterId :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {primaryClusterId = a} :: CreateReplicationGroup)
{-# DEPRECATED crgPrimaryClusterId "Use generic-lens or generic-optics with 'primaryClusterId' instead." #-}

-- | The name of the cache engine to be used for the clusters in this replication group.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgEngine :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgEngine = Lens.lens (engine :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: CreateReplicationGroup)
{-# DEPRECATED crgEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:
--
-- Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
-- Valid values for @ddd@ are:
--
--     * @sun@
--
--
--     * @mon@
--
--
--     * @tue@
--
--
--     * @wed@
--
--
--     * @thu@
--
--
--     * @fri@
--
--
--     * @sat@
--
--
-- Example: @sun:23:00-mon:01:30@
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgPreferredMaintenanceWindow :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateReplicationGroup)
{-# DEPRECATED crgPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The ID of the KMS key used to encrypt the disk in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgKMSKeyId :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgKMSKeyId = Lens.lens (kmsKeyId :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateReplicationGroup)
{-# DEPRECATED crgKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
--
-- /Note:/ Consider using 'multiAZEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgMultiAZEnabled :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Bool)
crgMultiAZEnabled = Lens.lens (multiAZEnabled :: CreateReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZEnabled = a} :: CreateReplicationGroup)
{-# DEPRECATED crgMultiAZEnabled "Use generic-lens or generic-optics with 'multiAZEnabled' instead." #-}

-- | The name of the cache subnet group to be used for the replication group.
--
-- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgCacheSubnetGroupName :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: CreateReplicationGroup)
{-# DEPRECATED crgCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | An optional parameter that specifies the number of node groups (shards) for this Redis (cluster mode enabled) replication group. For Redis (cluster mode disabled) either omit this parameter or set it to 1.
--
-- Default: 1
--
-- /Note:/ Consider using 'numNodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNumNodeGroups :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Int)
crgNumNodeGroups = Lens.lens (numNodeGroups :: CreateReplicationGroup -> Lude.Maybe Lude.Int) (\s a -> s {numNodeGroups = a} :: CreateReplicationGroup)
{-# DEPRECATED crgNumNodeGroups "Use generic-lens or generic-optics with 'numNodeGroups' instead." #-}

-- | The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cluster).
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotRetentionLimit :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Int)
crgSnapshotRetentionLimit = Lens.lens (snapshotRetentionLimit :: CreateReplicationGroup -> Lude.Maybe Lude.Int) (\s a -> s {snapshotRetentionLimit = a} :: CreateReplicationGroup)
{-# DEPRECATED crgSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgGlobalReplicationGroupId :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgGlobalReplicationGroupId = Lens.lens (globalReplicationGroupId :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {globalReplicationGroupId = a} :: CreateReplicationGroup)
{-# DEPRECATED crgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | An optional parameter that specifies the number of replica nodes in each node group (shard). Valid values are 0 to 5.
--
-- /Note:/ Consider using 'replicasPerNodeGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgReplicasPerNodeGroup :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Int)
crgReplicasPerNodeGroup = Lens.lens (replicasPerNodeGroup :: CreateReplicationGroup -> Lude.Maybe Lude.Int) (\s a -> s {replicasPerNodeGroup = a} :: CreateReplicationGroup)
{-# DEPRECATED crgReplicasPerNodeGroup "Use generic-lens or generic-optics with 'replicasPerNodeGroup' instead." #-}

-- | The number of clusters this replication group initially has.
--
-- This parameter is not used if there is more than one node group (shard). You should use @ReplicasPerNodeGroup@ instead.
-- If @AutomaticFailoverEnabled@ is @true@ , the value of this parameter must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can omit this parameter (it will default to 1), or you can explicitly set it to a value between 2 and 6.
-- The maximum permitted value for @NumCacheClusters@ is 6 (1 primary plus 5 replicas).
--
-- /Note:/ Consider using 'numCacheClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNumCacheClusters :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Int)
crgNumCacheClusters = Lens.lens (numCacheClusters :: CreateReplicationGroup -> Lude.Maybe Lude.Int) (\s a -> s {numCacheClusters = a} :: CreateReplicationGroup)
{-# DEPRECATED crgNumCacheClusters "Use generic-lens or generic-optics with 'numCacheClusters' instead." #-}

-- | A list of EC2 Availability Zones in which the replication group's clusters are created. The order of the Availability Zones in the list is the order in which clusters are allocated. The primary cluster is created in the first AZ in the list.
--
-- This parameter is not used if there is more than one node group (shard). You should use @NodeGroupConfiguration@ instead.
-- Default: system chosen Availability Zones.
--
-- /Note:/ Consider using 'preferredCacheClusterAZs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgPreferredCacheClusterAZs :: Lens.Lens' CreateReplicationGroup (Lude.Maybe [Lude.Text])
crgPreferredCacheClusterAZs = Lens.lens (preferredCacheClusterAZs :: CreateReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {preferredCacheClusterAZs = a} :: CreateReplicationGroup)
{-# DEPRECATED crgPreferredCacheClusterAZs "Use generic-lens or generic-optics with 'preferredCacheClusterAZs' instead." #-}

-- | The name of a snapshot from which to restore data into the new replication group. The snapshot status changes to @restoring@ while the new replication group is being created.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotName :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgSnapshotName = Lens.lens (snapshotName :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {snapshotName = a} :: CreateReplicationGroup)
{-# DEPRECATED crgSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNotificationTopicARN :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Text)
crgNotificationTopicARN = Lens.lens (notificationTopicARN :: CreateReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicARN = a} :: CreateReplicationGroup)
{-# DEPRECATED crgNotificationTopicARN "Use generic-lens or generic-optics with 'notificationTopicARN' instead." #-}

-- | A list of cost allocation tags to be added to this resource. Tags are comma-separated key,value pairs (e.g. Key=@myKey@ , Value=@myKeyValue@ . You can include multiple tags as shown following: Key=@myKey@ , Value=@myKeyValue@ Key=@mySecondKey@ , Value=@mySecondKeyValue@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgTags :: Lens.Lens' CreateReplicationGroup (Lude.Maybe [Tag])
crgTags = Lens.lens (tags :: CreateReplicationGroup -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateReplicationGroup)
{-# DEPRECATED crgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which each member of the replication group accepts connections.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgPort :: Lens.Lens' CreateReplicationGroup (Lude.Maybe Lude.Int)
crgPort = Lens.lens (port :: CreateReplicationGroup -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CreateReplicationGroup)
{-# DEPRECATED crgPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A list of cache security group names to associate with this replication group.
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgCacheSecurityGroupNames :: Lens.Lens' CreateReplicationGroup (Lude.Maybe [Lude.Text])
crgCacheSecurityGroupNames = Lens.lens (cacheSecurityGroupNames :: CreateReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheSecurityGroupNames = a} :: CreateReplicationGroup)
{-# DEPRECATED crgCacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead." #-}

-- | The replication group identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * A name must contain from 1 to 40 alphanumeric characters or hyphens.
--
--
--     * The first character must be a letter.
--
--
--     * A name cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgReplicationGroupId :: Lens.Lens' CreateReplicationGroup Lude.Text
crgReplicationGroupId = Lens.lens (replicationGroupId :: CreateReplicationGroup -> Lude.Text) (\s a -> s {replicationGroupId = a} :: CreateReplicationGroup)
{-# DEPRECATED crgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | A user-created description for the replication group.
--
-- /Note:/ Consider using 'replicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgReplicationGroupDescription :: Lens.Lens' CreateReplicationGroup Lude.Text
crgReplicationGroupDescription = Lens.lens (replicationGroupDescription :: CreateReplicationGroup -> Lude.Text) (\s a -> s {replicationGroupDescription = a} :: CreateReplicationGroup)
{-# DEPRECATED crgReplicationGroupDescription "Use generic-lens or generic-optics with 'replicationGroupDescription' instead." #-}

instance Lude.AWSRequest CreateReplicationGroup where
  type Rs CreateReplicationGroup = CreateReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateReplicationGroupResult"
      ( \s h x ->
          CreateReplicationGroupResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReplicationGroup where
  toQuery CreateReplicationGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "AutomaticFailoverEnabled" Lude.=: automaticFailoverEnabled,
        "EngineVersion" Lude.=: engineVersion,
        "CacheNodeType" Lude.=: cacheNodeType,
        "NodeGroupConfiguration"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "NodeGroupConfiguration"
                Lude.<$> nodeGroupConfiguration
            ),
        "AtRestEncryptionEnabled" Lude.=: atRestEncryptionEnabled,
        "SecurityGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        "SnapshotArns"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "SnapshotArn" Lude.<$> snapshotARNs),
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "TransitEncryptionEnabled" Lude.=: transitEncryptionEnabled,
        "UserGroupIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> userGroupIds),
        "SnapshotWindow" Lude.=: snapshotWindow,
        "AuthToken" Lude.=: authToken,
        "PrimaryClusterId" Lude.=: primaryClusterId,
        "Engine" Lude.=: engine,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "KmsKeyId" Lude.=: kmsKeyId,
        "MultiAZEnabled" Lude.=: multiAZEnabled,
        "CacheSubnetGroupName" Lude.=: cacheSubnetGroupName,
        "NumNodeGroups" Lude.=: numNodeGroups,
        "SnapshotRetentionLimit" Lude.=: snapshotRetentionLimit,
        "GlobalReplicationGroupId" Lude.=: globalReplicationGroupId,
        "ReplicasPerNodeGroup" Lude.=: replicasPerNodeGroup,
        "NumCacheClusters" Lude.=: numCacheClusters,
        "PreferredCacheClusterAZs"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "AvailabilityZone"
                Lude.<$> preferredCacheClusterAZs
            ),
        "SnapshotName" Lude.=: snapshotName,
        "NotificationTopicArn" Lude.=: notificationTopicARN,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "CacheSecurityGroupNames"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "CacheSecurityGroupName"
                Lude.<$> cacheSecurityGroupNames
            ),
        "ReplicationGroupId" Lude.=: replicationGroupId,
        "ReplicationGroupDescription" Lude.=: replicationGroupDescription
      ]

-- | /See:/ 'mkCreateReplicationGroupResponse' smart constructor.
data CreateReplicationGroupResponse = CreateReplicationGroupResponse'
  { replicationGroup ::
      Lude.Maybe ReplicationGroup,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReplicationGroupResponse
mkCreateReplicationGroupResponse pResponseStatus_ =
  CreateReplicationGroupResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsReplicationGroup :: Lens.Lens' CreateReplicationGroupResponse (Lude.Maybe ReplicationGroup)
crgrsReplicationGroup = Lens.lens (replicationGroup :: CreateReplicationGroupResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: CreateReplicationGroupResponse)
{-# DEPRECATED crgrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrsResponseStatus :: Lens.Lens' CreateReplicationGroupResponse Lude.Int
crgrsResponseStatus = Lens.lens (responseStatus :: CreateReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReplicationGroupResponse)
{-# DEPRECATED crgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
