{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    crgReplicationGroupId,
    crgReplicationGroupDescription,
    crgAtRestEncryptionEnabled,
    crgAuthToken,
    crgAutoMinorVersionUpgrade,
    crgAutomaticFailoverEnabled,
    crgCacheNodeType,
    crgCacheParameterGroupName,
    crgCacheSecurityGroupNames,
    crgCacheSubnetGroupName,
    crgEngine,
    crgEngineVersion,
    crgGlobalReplicationGroupId,
    crgKmsKeyId,
    crgMultiAZEnabled,
    crgNodeGroupConfiguration,
    crgNotificationTopicArn,
    crgNumCacheClusters,
    crgNumNodeGroups,
    crgPort,
    crgPreferredCacheClusterAZs,
    crgPreferredMaintenanceWindow,
    crgPrimaryClusterId,
    crgReplicasPerNodeGroup,
    crgSecurityGroupIds,
    crgSnapshotArns,
    crgSnapshotName,
    crgSnapshotRetentionLimit,
    crgSnapshotWindow,
    crgTags,
    crgTransitEncryptionEnabled,
    crgUserGroupIds,

    -- * Destructuring the response
    CreateReplicationGroupResponse (..),
    mkCreateReplicationGroupResponse,

    -- ** Response lenses
    crgrrsReplicationGroup,
    crgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateReplicationGroup@ operation.
--
-- /See:/ 'mkCreateReplicationGroup' smart constructor.
data CreateReplicationGroup = CreateReplicationGroup'
  { -- | The replication group identifier. This parameter is stored as a lowercase string.
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
    replicationGroupId :: Types.ReplicationGroupId,
    -- | A user-created description for the replication group.
    replicationGroupDescription :: Types.ReplicationGroupDescription,
    -- | A flag that enables encryption at rest when set to @true@ .
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
    -- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
    -- Default: @false@
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
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
    authToken :: Core.Maybe Types.AuthToken,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | Specifies whether a read-only replica is automatically promoted to read/write primary if the existing primary fails.
    --
    -- @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode enabled) replication groups.
    -- Default: false
    automaticFailoverEnabled :: Core.Maybe Core.Bool,
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
    cacheNodeType :: Core.Maybe Types.CacheNodeType,
    -- | The name of the parameter group to associate with this replication group. If this argument is omitted, the default cache parameter group for the specified engine is used.
    --
    -- If you are running Redis version 3.2.4 or later, only one node group (shard), and want to use a default parameter group, we recommend that you specify the parameter group by name.
    --
    --     * To create a Redis (cluster mode disabled) replication group, use @CacheParameterGroupName=default.redis3.2@ .
    --
    --
    --     * To create a Redis (cluster mode enabled) replication group, use @CacheParameterGroupName=default.redis3.2.cluster.on@ .
    cacheParameterGroupName :: Core.Maybe Types.CacheParameterGroupName,
    -- | A list of cache security group names to associate with this replication group.
    cacheSecurityGroupNames :: Core.Maybe [Types.String],
    -- | The name of the cache subnet group to be used for the replication group.
    --
    -- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
    cacheSubnetGroupName :: Core.Maybe Types.CacheSubnetGroupName,
    -- | The name of the cache engine to be used for the clusters in this replication group.
    engine :: Core.Maybe Types.Engine,
    -- | The version number of the cache engine to be used for the clusters in this replication group. To view the supported cache engine versions, use the @DescribeCacheEngineVersions@ operation.
    --
    -- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ) in the /ElastiCache User Guide/ , but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
    engineVersion :: Core.Maybe Types.EngineVersion,
    -- | The name of the Global Datastore
    globalReplicationGroupId :: Core.Maybe Types.GlobalReplicationGroupId,
    -- | The ID of the KMS key used to encrypt the disk in the cluster.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
    multiAZEnabled :: Core.Maybe Core.Bool,
    -- | A list of node group (shard) configuration options. Each node group (shard) configuration has the following members: @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ , and @Slots@ .
    --
    -- If you're creating a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group, you can use this parameter to individually configure each node group (shard), or you can omit this parameter. However, it is required when seeding a Redis (cluster mode enabled) cluster from a S3 rdb file. You must configure each node group (shard) using this parameter because you must specify the slots for each node group.
    nodeGroupConfiguration :: Core.Maybe [Types.NodeGroupConfiguration],
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
    notificationTopicArn :: Core.Maybe Types.NotificationTopicArn,
    -- | The number of clusters this replication group initially has.
    --
    -- This parameter is not used if there is more than one node group (shard). You should use @ReplicasPerNodeGroup@ instead.
    -- If @AutomaticFailoverEnabled@ is @true@ , the value of this parameter must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can omit this parameter (it will default to 1), or you can explicitly set it to a value between 2 and 6.
    -- The maximum permitted value for @NumCacheClusters@ is 6 (1 primary plus 5 replicas).
    numCacheClusters :: Core.Maybe Core.Int,
    -- | An optional parameter that specifies the number of node groups (shards) for this Redis (cluster mode enabled) replication group. For Redis (cluster mode disabled) either omit this parameter or set it to 1.
    --
    -- Default: 1
    numNodeGroups :: Core.Maybe Core.Int,
    -- | The port number on which each member of the replication group accepts connections.
    port :: Core.Maybe Core.Int,
    -- | A list of EC2 Availability Zones in which the replication group's clusters are created. The order of the Availability Zones in the list is the order in which clusters are allocated. The primary cluster is created in the first AZ in the list.
    --
    -- This parameter is not used if there is more than one node group (shard). You should use @NodeGroupConfiguration@ instead.
    -- Default: system chosen Availability Zones.
    preferredCacheClusterAZs :: Core.Maybe [Types.String],
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
    preferredMaintenanceWindow :: Core.Maybe Types.PreferredMaintenanceWindow,
    -- | The identifier of the cluster that serves as the primary for this replication group. This cluster must already exist and have a status of @available@ .
    --
    -- This parameter is not required if @NumCacheClusters@ , @NumNodeGroups@ , or @ReplicasPerNodeGroup@ is specified.
    primaryClusterId :: Core.Maybe Types.PrimaryClusterId,
    -- | An optional parameter that specifies the number of replica nodes in each node group (shard). Valid values are 0 to 5.
    replicasPerNodeGroup :: Core.Maybe Core.Int,
    -- | One or more Amazon VPC security groups associated with this replication group.
    --
    -- Use this parameter only when you are creating a replication group in an Amazon Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Core.Maybe [Types.String],
    -- | A list of Amazon Resource Names (ARN) that uniquely identify the Redis RDB snapshot files stored in Amazon S3. The snapshot files are used to populate the new replication group. The Amazon S3 object name in the ARN cannot contain any commas. The new replication group will have the number of node groups (console: shards) specified by the parameter /NumNodeGroups/ or the number of node groups configured by /NodeGroupConfiguration/ regardless of the number of ARNs specified here.
    --
    -- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
    snapshotArns :: Core.Maybe [Types.String],
    -- | The name of a snapshot from which to restore data into the new replication group. The snapshot status changes to @restoring@ while the new replication group is being created.
    snapshotName :: Core.Maybe Types.SnapshotName,
    -- | The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
    --
    -- Default: 0 (i.e., automatic backups are disabled for this cluster).
    snapshotRetentionLimit :: Core.Maybe Core.Int,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
    --
    -- Example: @05:00-09:00@
    -- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
    snapshotWindow :: Core.Maybe Types.SnapshotWindow,
    -- | A list of cost allocation tags to be added to this resource. Tags are comma-separated key,value pairs (e.g. Key=@myKey@ , Value=@myKeyValue@ . You can include multiple tags as shown following: Key=@myKey@ , Value=@myKeyValue@ Key=@mySecondKey@ , Value=@mySecondKeyValue@ .
    tags :: Core.Maybe [Types.Tag],
    -- | A flag that enables in-transit encryption when set to @true@ .
    --
    -- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
    -- This parameter is valid only if the @Engine@ parameter is @redis@ , the @EngineVersion@ parameter is @3.2.6@ , @4.x@ or later, and the cluster is being created in an Amazon VPC.
    -- If you enable in-transit encryption, you must also specify a value for @CacheSubnetGroup@ .
    -- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
    -- Default: @false@
    -- /Important:/ For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as @true@ , an @AuthToken@ , and a @CacheSubnetGroup@ .
    transitEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | The list of user groups to associate with the replication group.
    userGroupIds :: Core.Maybe (Core.NonEmpty Types.UserGroupId)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateReplicationGroup' value with any optional fields omitted.
mkCreateReplicationGroup ::
  -- | 'replicationGroupId'
  Types.ReplicationGroupId ->
  -- | 'replicationGroupDescription'
  Types.ReplicationGroupDescription ->
  CreateReplicationGroup
mkCreateReplicationGroup
  replicationGroupId
  replicationGroupDescription =
    CreateReplicationGroup'
      { replicationGroupId,
        replicationGroupDescription,
        atRestEncryptionEnabled = Core.Nothing,
        authToken = Core.Nothing,
        autoMinorVersionUpgrade = Core.Nothing,
        automaticFailoverEnabled = Core.Nothing,
        cacheNodeType = Core.Nothing,
        cacheParameterGroupName = Core.Nothing,
        cacheSecurityGroupNames = Core.Nothing,
        cacheSubnetGroupName = Core.Nothing,
        engine = Core.Nothing,
        engineVersion = Core.Nothing,
        globalReplicationGroupId = Core.Nothing,
        kmsKeyId = Core.Nothing,
        multiAZEnabled = Core.Nothing,
        nodeGroupConfiguration = Core.Nothing,
        notificationTopicArn = Core.Nothing,
        numCacheClusters = Core.Nothing,
        numNodeGroups = Core.Nothing,
        port = Core.Nothing,
        preferredCacheClusterAZs = Core.Nothing,
        preferredMaintenanceWindow = Core.Nothing,
        primaryClusterId = Core.Nothing,
        replicasPerNodeGroup = Core.Nothing,
        securityGroupIds = Core.Nothing,
        snapshotArns = Core.Nothing,
        snapshotName = Core.Nothing,
        snapshotRetentionLimit = Core.Nothing,
        snapshotWindow = Core.Nothing,
        tags = Core.Nothing,
        transitEncryptionEnabled = Core.Nothing,
        userGroupIds = Core.Nothing
      }

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
crgReplicationGroupId :: Lens.Lens' CreateReplicationGroup Types.ReplicationGroupId
crgReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED crgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | A user-created description for the replication group.
--
-- /Note:/ Consider using 'replicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgReplicationGroupDescription :: Lens.Lens' CreateReplicationGroup Types.ReplicationGroupDescription
crgReplicationGroupDescription = Lens.field @"replicationGroupDescription"
{-# DEPRECATED crgReplicationGroupDescription "Use generic-lens or generic-optics with 'replicationGroupDescription' instead." #-}

-- | A flag that enables encryption at rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the replication group is created. To enable encryption at rest on a replication group you must set @AtRestEncryptionEnabled@ to @true@ when you create the replication group.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgAtRestEncryptionEnabled :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Bool)
crgAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# DEPRECATED crgAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

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
crgAuthToken :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.AuthToken)
crgAuthToken = Lens.field @"authToken"
{-# DEPRECATED crgAuthToken "Use generic-lens or generic-optics with 'authToken' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgAutoMinorVersionUpgrade :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Bool)
crgAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED crgAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | Specifies whether a read-only replica is automatically promoted to read/write primary if the existing primary fails.
--
-- @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode enabled) replication groups.
-- Default: false
--
-- /Note:/ Consider using 'automaticFailoverEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgAutomaticFailoverEnabled :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Bool)
crgAutomaticFailoverEnabled = Lens.field @"automaticFailoverEnabled"
{-# DEPRECATED crgAutomaticFailoverEnabled "Use generic-lens or generic-optics with 'automaticFailoverEnabled' instead." #-}

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
crgCacheNodeType :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.CacheNodeType)
crgCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED crgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

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
crgCacheParameterGroupName :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.CacheParameterGroupName)
crgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED crgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | A list of cache security group names to associate with this replication group.
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgCacheSecurityGroupNames :: Lens.Lens' CreateReplicationGroup (Core.Maybe [Types.String])
crgCacheSecurityGroupNames = Lens.field @"cacheSecurityGroupNames"
{-# DEPRECATED crgCacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead." #-}

-- | The name of the cache subnet group to be used for the replication group.
--
-- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgCacheSubnetGroupName :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.CacheSubnetGroupName)
crgCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# DEPRECATED crgCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | The name of the cache engine to be used for the clusters in this replication group.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgEngine :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.Engine)
crgEngine = Lens.field @"engine"
{-# DEPRECATED crgEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version number of the cache engine to be used for the clusters in this replication group. To view the supported cache engine versions, use the @DescribeCacheEngineVersions@ operation.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ) in the /ElastiCache User Guide/ , but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgEngineVersion :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.EngineVersion)
crgEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED crgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgGlobalReplicationGroupId :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.GlobalReplicationGroupId)
crgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED crgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The ID of the KMS key used to encrypt the disk in the cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgKmsKeyId :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.KmsKeyId)
crgKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED crgKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
--
-- /Note:/ Consider using 'multiAZEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgMultiAZEnabled :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Bool)
crgMultiAZEnabled = Lens.field @"multiAZEnabled"
{-# DEPRECATED crgMultiAZEnabled "Use generic-lens or generic-optics with 'multiAZEnabled' instead." #-}

-- | A list of node group (shard) configuration options. Each node group (shard) configuration has the following members: @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ , and @Slots@ .
--
-- If you're creating a Redis (cluster mode disabled) or a Redis (cluster mode enabled) replication group, you can use this parameter to individually configure each node group (shard), or you can omit this parameter. However, it is required when seeding a Redis (cluster mode enabled) cluster from a S3 rdb file. You must configure each node group (shard) using this parameter because you must specify the slots for each node group.
--
-- /Note:/ Consider using 'nodeGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNodeGroupConfiguration :: Lens.Lens' CreateReplicationGroup (Core.Maybe [Types.NodeGroupConfiguration])
crgNodeGroupConfiguration = Lens.field @"nodeGroupConfiguration"
{-# DEPRECATED crgNodeGroupConfiguration "Use generic-lens or generic-optics with 'nodeGroupConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNotificationTopicArn :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.NotificationTopicArn)
crgNotificationTopicArn = Lens.field @"notificationTopicArn"
{-# DEPRECATED crgNotificationTopicArn "Use generic-lens or generic-optics with 'notificationTopicArn' instead." #-}

-- | The number of clusters this replication group initially has.
--
-- This parameter is not used if there is more than one node group (shard). You should use @ReplicasPerNodeGroup@ instead.
-- If @AutomaticFailoverEnabled@ is @true@ , the value of this parameter must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can omit this parameter (it will default to 1), or you can explicitly set it to a value between 2 and 6.
-- The maximum permitted value for @NumCacheClusters@ is 6 (1 primary plus 5 replicas).
--
-- /Note:/ Consider using 'numCacheClusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNumCacheClusters :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Int)
crgNumCacheClusters = Lens.field @"numCacheClusters"
{-# DEPRECATED crgNumCacheClusters "Use generic-lens or generic-optics with 'numCacheClusters' instead." #-}

-- | An optional parameter that specifies the number of node groups (shards) for this Redis (cluster mode enabled) replication group. For Redis (cluster mode disabled) either omit this parameter or set it to 1.
--
-- Default: 1
--
-- /Note:/ Consider using 'numNodeGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgNumNodeGroups :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Int)
crgNumNodeGroups = Lens.field @"numNodeGroups"
{-# DEPRECATED crgNumNodeGroups "Use generic-lens or generic-optics with 'numNodeGroups' instead." #-}

-- | The port number on which each member of the replication group accepts connections.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgPort :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Int)
crgPort = Lens.field @"port"
{-# DEPRECATED crgPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A list of EC2 Availability Zones in which the replication group's clusters are created. The order of the Availability Zones in the list is the order in which clusters are allocated. The primary cluster is created in the first AZ in the list.
--
-- This parameter is not used if there is more than one node group (shard). You should use @NodeGroupConfiguration@ instead.
-- Default: system chosen Availability Zones.
--
-- /Note:/ Consider using 'preferredCacheClusterAZs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgPreferredCacheClusterAZs :: Lens.Lens' CreateReplicationGroup (Core.Maybe [Types.String])
crgPreferredCacheClusterAZs = Lens.field @"preferredCacheClusterAZs"
{-# DEPRECATED crgPreferredCacheClusterAZs "Use generic-lens or generic-optics with 'preferredCacheClusterAZs' instead." #-}

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
crgPreferredMaintenanceWindow :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.PreferredMaintenanceWindow)
crgPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED crgPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The identifier of the cluster that serves as the primary for this replication group. This cluster must already exist and have a status of @available@ .
--
-- This parameter is not required if @NumCacheClusters@ , @NumNodeGroups@ , or @ReplicasPerNodeGroup@ is specified.
--
-- /Note:/ Consider using 'primaryClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgPrimaryClusterId :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.PrimaryClusterId)
crgPrimaryClusterId = Lens.field @"primaryClusterId"
{-# DEPRECATED crgPrimaryClusterId "Use generic-lens or generic-optics with 'primaryClusterId' instead." #-}

-- | An optional parameter that specifies the number of replica nodes in each node group (shard). Valid values are 0 to 5.
--
-- /Note:/ Consider using 'replicasPerNodeGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgReplicasPerNodeGroup :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Int)
crgReplicasPerNodeGroup = Lens.field @"replicasPerNodeGroup"
{-# DEPRECATED crgReplicasPerNodeGroup "Use generic-lens or generic-optics with 'replicasPerNodeGroup' instead." #-}

-- | One or more Amazon VPC security groups associated with this replication group.
--
-- Use this parameter only when you are creating a replication group in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSecurityGroupIds :: Lens.Lens' CreateReplicationGroup (Core.Maybe [Types.String])
crgSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED crgSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A list of Amazon Resource Names (ARN) that uniquely identify the Redis RDB snapshot files stored in Amazon S3. The snapshot files are used to populate the new replication group. The Amazon S3 object name in the ARN cannot contain any commas. The new replication group will have the number of node groups (console: shards) specified by the parameter /NumNodeGroups/ or the number of node groups configured by /NodeGroupConfiguration/ regardless of the number of ARNs specified here.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
--
-- /Note:/ Consider using 'snapshotArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotArns :: Lens.Lens' CreateReplicationGroup (Core.Maybe [Types.String])
crgSnapshotArns = Lens.field @"snapshotArns"
{-# DEPRECATED crgSnapshotArns "Use generic-lens or generic-optics with 'snapshotArns' instead." #-}

-- | The name of a snapshot from which to restore data into the new replication group. The snapshot status changes to @restoring@ while the new replication group is being created.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotName :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.SnapshotName)
crgSnapshotName = Lens.field @"snapshotName"
{-# DEPRECATED crgSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cluster).
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotRetentionLimit :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Int)
crgSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# DEPRECATED crgSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgSnapshotWindow :: Lens.Lens' CreateReplicationGroup (Core.Maybe Types.SnapshotWindow)
crgSnapshotWindow = Lens.field @"snapshotWindow"
{-# DEPRECATED crgSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | A list of cost allocation tags to be added to this resource. Tags are comma-separated key,value pairs (e.g. Key=@myKey@ , Value=@myKeyValue@ . You can include multiple tags as shown following: Key=@myKey@ , Value=@myKeyValue@ Key=@mySecondKey@ , Value=@mySecondKeyValue@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgTags :: Lens.Lens' CreateReplicationGroup (Core.Maybe [Types.Tag])
crgTags = Lens.field @"tags"
{-# DEPRECATED crgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
crgTransitEncryptionEnabled :: Lens.Lens' CreateReplicationGroup (Core.Maybe Core.Bool)
crgTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# DEPRECATED crgTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

-- | The list of user groups to associate with the replication group.
--
-- /Note:/ Consider using 'userGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgUserGroupIds :: Lens.Lens' CreateReplicationGroup (Core.Maybe (Core.NonEmpty Types.UserGroupId))
crgUserGroupIds = Lens.field @"userGroupIds"
{-# DEPRECATED crgUserGroupIds "Use generic-lens or generic-optics with 'userGroupIds' instead." #-}

instance Core.AWSRequest CreateReplicationGroup where
  type Rs CreateReplicationGroup = CreateReplicationGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> ( Core.toQueryValue
                            "ReplicationGroupDescription"
                            replicationGroupDescription
                        )
                Core.<> ( Core.toQueryValue "AtRestEncryptionEnabled"
                            Core.<$> atRestEncryptionEnabled
                        )
                Core.<> (Core.toQueryValue "AuthToken" Core.<$> authToken)
                Core.<> ( Core.toQueryValue "AutoMinorVersionUpgrade"
                            Core.<$> autoMinorVersionUpgrade
                        )
                Core.<> ( Core.toQueryValue "AutomaticFailoverEnabled"
                            Core.<$> automaticFailoverEnabled
                        )
                Core.<> (Core.toQueryValue "CacheNodeType" Core.<$> cacheNodeType)
                Core.<> ( Core.toQueryValue "CacheParameterGroupName"
                            Core.<$> cacheParameterGroupName
                        )
                Core.<> ( Core.toQueryValue
                            "CacheSecurityGroupNames"
                            ( Core.toQueryList "CacheSecurityGroupName"
                                Core.<$> cacheSecurityGroupNames
                            )
                        )
                Core.<> ( Core.toQueryValue "CacheSubnetGroupName"
                            Core.<$> cacheSubnetGroupName
                        )
                Core.<> (Core.toQueryValue "Engine" Core.<$> engine)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> ( Core.toQueryValue "GlobalReplicationGroupId"
                            Core.<$> globalReplicationGroupId
                        )
                Core.<> (Core.toQueryValue "KmsKeyId" Core.<$> kmsKeyId)
                Core.<> (Core.toQueryValue "MultiAZEnabled" Core.<$> multiAZEnabled)
                Core.<> ( Core.toQueryValue
                            "NodeGroupConfiguration"
                            ( Core.toQueryList "NodeGroupConfiguration"
                                Core.<$> nodeGroupConfiguration
                            )
                        )
                Core.<> ( Core.toQueryValue "NotificationTopicArn"
                            Core.<$> notificationTopicArn
                        )
                Core.<> (Core.toQueryValue "NumCacheClusters" Core.<$> numCacheClusters)
                Core.<> (Core.toQueryValue "NumNodeGroups" Core.<$> numNodeGroups)
                Core.<> (Core.toQueryValue "Port" Core.<$> port)
                Core.<> ( Core.toQueryValue
                            "PreferredCacheClusterAZs"
                            ( Core.toQueryList "AvailabilityZone"
                                Core.<$> preferredCacheClusterAZs
                            )
                        )
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> (Core.toQueryValue "PrimaryClusterId" Core.<$> primaryClusterId)
                Core.<> ( Core.toQueryValue "ReplicasPerNodeGroup"
                            Core.<$> replicasPerNodeGroup
                        )
                Core.<> ( Core.toQueryValue
                            "SecurityGroupIds"
                            (Core.toQueryList "SecurityGroupId" Core.<$> securityGroupIds)
                        )
                Core.<> ( Core.toQueryValue
                            "SnapshotArns"
                            (Core.toQueryList "SnapshotArn" Core.<$> snapshotArns)
                        )
                Core.<> (Core.toQueryValue "SnapshotName" Core.<$> snapshotName)
                Core.<> ( Core.toQueryValue "SnapshotRetentionLimit"
                            Core.<$> snapshotRetentionLimit
                        )
                Core.<> (Core.toQueryValue "SnapshotWindow" Core.<$> snapshotWindow)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
                Core.<> ( Core.toQueryValue "TransitEncryptionEnabled"
                            Core.<$> transitEncryptionEnabled
                        )
                Core.<> ( Core.toQueryValue
                            "UserGroupIds"
                            (Core.toQueryList "member" Core.<$> userGroupIds)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateReplicationGroupResult"
      ( \s h x ->
          CreateReplicationGroupResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateReplicationGroupResponse' smart constructor.
data CreateReplicationGroupResponse = CreateReplicationGroupResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateReplicationGroupResponse' value with any optional fields omitted.
mkCreateReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateReplicationGroupResponse
mkCreateReplicationGroupResponse responseStatus =
  CreateReplicationGroupResponse'
    { replicationGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsReplicationGroup :: Lens.Lens' CreateReplicationGroupResponse (Core.Maybe Types.ReplicationGroup)
crgrrsReplicationGroup = Lens.field @"replicationGroup"
{-# DEPRECATED crgrrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crgrrsResponseStatus :: Lens.Lens' CreateReplicationGroupResponse Core.Int
crgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
