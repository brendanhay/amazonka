{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cluster. All nodes in the cluster run the same protocol-compliant cache engine software, either Memcached or Redis.
--
-- This operation is not supported for Redis (cluster mode enabled) clusters.
module Network.AWS.ElastiCache.CreateCacheCluster
  ( -- * Creating a request
    CreateCacheCluster (..),
    mkCreateCacheCluster,

    -- ** Request lenses
    cccEngineVersion,
    cccCacheNodeType,
    cccSecurityGroupIds,
    cccSnapshotARNs,
    cccAutoMinorVersionUpgrade,
    cccOutpostMode,
    cccCacheParameterGroupName,
    cccSnapshotWindow,
    cccAuthToken,
    cccEngine,
    cccPreferredAvailabilityZones,
    cccPreferredMaintenanceWindow,
    cccCacheSubnetGroupName,
    cccPreferredAvailabilityZone,
    cccSnapshotRetentionLimit,
    cccPreferredOutpostARNs,
    cccAZMode,
    cccSnapshotName,
    cccPreferredOutpostARN,
    cccReplicationGroupId,
    cccNotificationTopicARN,
    cccNumCacheNodes,
    cccTags,
    cccPort,
    cccCacheSecurityGroupNames,
    cccCacheClusterId,

    -- * Destructuring the response
    CreateCacheClusterResponse (..),
    mkCreateCacheClusterResponse,

    -- ** Response lenses
    cccrsCacheCluster,
    cccrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a CreateCacheCluster operation.
--
-- /See:/ 'mkCreateCacheCluster' smart constructor.
data CreateCacheCluster = CreateCacheCluster'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    securityGroupIds :: Lude.Maybe [Lude.Text],
    snapshotARNs :: Lude.Maybe [Lude.Text],
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    outpostMode :: Lude.Maybe OutpostMode,
    cacheParameterGroupName :: Lude.Maybe Lude.Text,
    snapshotWindow :: Lude.Maybe Lude.Text,
    authToken :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    preferredAvailabilityZones :: Lude.Maybe [Lude.Text],
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    cacheSubnetGroupName :: Lude.Maybe Lude.Text,
    preferredAvailabilityZone :: Lude.Maybe Lude.Text,
    snapshotRetentionLimit :: Lude.Maybe Lude.Int,
    preferredOutpostARNs :: Lude.Maybe [Lude.Text],
    aZMode :: Lude.Maybe AZMode,
    snapshotName :: Lude.Maybe Lude.Text,
    preferredOutpostARN :: Lude.Maybe Lude.Text,
    replicationGroupId :: Lude.Maybe Lude.Text,
    notificationTopicARN :: Lude.Maybe Lude.Text,
    numCacheNodes :: Lude.Maybe Lude.Int,
    tags :: Lude.Maybe [Tag],
    port :: Lude.Maybe Lude.Int,
    cacheSecurityGroupNames :: Lude.Maybe [Lude.Text],
    cacheClusterId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCacheCluster' with the minimum fields required to make a request.
--
-- * 'aZMode' - Specifies whether the nodes in this Memcached cluster are created in a single Availability Zone or created across multiple Availability Zones in the cluster's region.
--
-- This parameter is only supported for Memcached clusters.
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified, ElastiCache assumes @single-az@ mode.
-- * 'authToken' - __Reserved parameter.__ The password used to access a password protected server.
--
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
-- * 'cacheClusterId' - The node group (shard) identifier. This parameter is stored as a lowercase string.
--
-- __Constraints:__
--
--     * A name must contain from 1 to 50 alphanumeric characters or hyphens.
--
--
--     * The first character must be a letter.
--
--
--     * A name cannot end with a hyphen or contain two consecutive hyphens.
--
--
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
-- * 'cacheParameterGroupName' - The name of the parameter group to associate with this cluster. If this argument is omitted, the default parameter group for the specified engine is used. You cannot use any parameter group which has @cluster-enabled='yes'@ when creating a cluster.
-- * 'cacheSecurityGroupNames' - A list of security group names to associate with this cluster.
--
-- Use this parameter only when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- * 'cacheSubnetGroupName' - The name of the subnet group to be used for the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
-- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
-- * 'engine' - The name of the cache engine to be used for this cluster.
--
-- Valid values for this parameter are: @memcached@ | @redis@
-- * 'engineVersion' - The version number of the cache engine to be used for this cluster. To view the supported cache engine versions, use the DescribeCacheEngineVersions operation.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
-- * 'notificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
-- * 'numCacheNodes' - The initial number of cache nodes that the cluster has.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
-- If you need more than 20 nodes for your Memcached cluster, please fill out the ElastiCache Limit Increase Request form at <http://aws.amazon.com/contact-us/elasticache-node-limit-request/ http://aws.amazon.com/contact-us/elasticache-node-limit-request/> .
-- * 'outpostMode' - Specifies whether the nodes in the cluster are created in a single outpost or across multiple outposts.
-- * 'port' - The port number on which each of the cache nodes accepts connections.
-- * 'preferredAvailabilityZone' - The EC2 Availability Zone in which the cluster is created.
--
-- All nodes belonging to this cluster are placed in the preferred Availability Zone. If you want to create your nodes across multiple Availability Zones, use @PreferredAvailabilityZones@ .
-- Default: System chosen Availability Zone.
-- * 'preferredAvailabilityZones' - A list of the Availability Zones in which cache nodes are created. The order of the zones in the list is not important.
--
-- This option is only supported on Memcached.
-- If you want all the nodes in the same Availability Zone, use @PreferredAvailabilityZone@ instead, or repeat the Availability Zone multiple times in the list.
-- Default: System chosen Availability Zones.
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
-- * 'preferredOutpostARN' - The outpost ARN in which the cache cluster is created.
-- * 'preferredOutpostARNs' - The outpost ARNs in which the cache cluster is created.
-- * 'replicationGroupId' - The ID of the replication group to which this cluster should belong. If this parameter is specified, the cluster is added to the specified replication group as a read replica; otherwise, the cluster is a standalone primary that is not part of any replication group.
--
-- If the specified replication group is Multi-AZ enabled and the Availability Zone is not specified, the cluster is created in Availability Zones that provide the best spread of read replicas across Availability Zones.
-- * 'securityGroupIds' - One or more VPC security groups associated with the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
-- * 'snapshotARNs' - A single-element string list containing an Amazon Resource Name (ARN) that uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The snapshot file is used to populate the node group (shard). The Amazon S3 object name in the ARN cannot contain any commas.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
-- * 'snapshotName' - The name of a Redis snapshot from which to restore data into the new node group (shard). The snapshot status changes to @restoring@ while the new node group (shard) is being created.
-- * 'snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot taken today is retained for 5 days before being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache cluster).
-- * 'snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
-- * 'tags' - A list of cost allocation tags to be added to this resource.
mkCreateCacheCluster ::
  -- | 'cacheClusterId'
  Lude.Text ->
  CreateCacheCluster
mkCreateCacheCluster pCacheClusterId_ =
  CreateCacheCluster'
    { engineVersion = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      snapshotARNs = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      outpostMode = Lude.Nothing,
      cacheParameterGroupName = Lude.Nothing,
      snapshotWindow = Lude.Nothing,
      authToken = Lude.Nothing,
      engine = Lude.Nothing,
      preferredAvailabilityZones = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      cacheSubnetGroupName = Lude.Nothing,
      preferredAvailabilityZone = Lude.Nothing,
      snapshotRetentionLimit = Lude.Nothing,
      preferredOutpostARNs = Lude.Nothing,
      aZMode = Lude.Nothing,
      snapshotName = Lude.Nothing,
      preferredOutpostARN = Lude.Nothing,
      replicationGroupId = Lude.Nothing,
      notificationTopicARN = Lude.Nothing,
      numCacheNodes = Lude.Nothing,
      tags = Lude.Nothing,
      port = Lude.Nothing,
      cacheSecurityGroupNames = Lude.Nothing,
      cacheClusterId = pCacheClusterId_
    }

-- | The version number of the cache engine to be used for this cluster. To view the supported cache engine versions, use the DescribeCacheEngineVersions operation.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccEngineVersion :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccEngineVersion = Lens.lens (engineVersion :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CreateCacheCluster)
{-# DEPRECATED cccEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

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
cccCacheNodeType :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccCacheNodeType = Lens.lens (cacheNodeType :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: CreateCacheCluster)
{-# DEPRECATED cccCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | One or more VPC security groups associated with the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSecurityGroupIds :: Lens.Lens' CreateCacheCluster (Lude.Maybe [Lude.Text])
cccSecurityGroupIds = Lens.lens (securityGroupIds :: CreateCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateCacheCluster)
{-# DEPRECATED cccSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A single-element string list containing an Amazon Resource Name (ARN) that uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The snapshot file is used to populate the node group (shard). The Amazon S3 object name in the ARN cannot contain any commas.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@
--
-- /Note:/ Consider using 'snapshotARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotARNs :: Lens.Lens' CreateCacheCluster (Lude.Maybe [Lude.Text])
cccSnapshotARNs = Lens.lens (snapshotARNs :: CreateCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {snapshotARNs = a} :: CreateCacheCluster)
{-# DEPRECATED cccSnapshotARNs "Use generic-lens or generic-optics with 'snapshotARNs' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccAutoMinorVersionUpgrade :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Bool)
cccAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: CreateCacheCluster -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: CreateCacheCluster)
{-# DEPRECATED cccAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | Specifies whether the nodes in the cluster are created in a single outpost or across multiple outposts.
--
-- /Note:/ Consider using 'outpostMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccOutpostMode :: Lens.Lens' CreateCacheCluster (Lude.Maybe OutpostMode)
cccOutpostMode = Lens.lens (outpostMode :: CreateCacheCluster -> Lude.Maybe OutpostMode) (\s a -> s {outpostMode = a} :: CreateCacheCluster)
{-# DEPRECATED cccOutpostMode "Use generic-lens or generic-optics with 'outpostMode' instead." #-}

-- | The name of the parameter group to associate with this cluster. If this argument is omitted, the default parameter group for the specified engine is used. You cannot use any parameter group which has @cluster-enabled='yes'@ when creating a cluster.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCacheParameterGroupName :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: CreateCacheCluster)
{-# DEPRECATED cccCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotWindow :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccSnapshotWindow = Lens.lens (snapshotWindow :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {snapshotWindow = a} :: CreateCacheCluster)
{-# DEPRECATED cccSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | __Reserved parameter.__ The password used to access a password protected server.
--
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
cccAuthToken :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccAuthToken = Lens.lens (authToken :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {authToken = a} :: CreateCacheCluster)
{-# DEPRECATED cccAuthToken "Use generic-lens or generic-optics with 'authToken' instead." #-}

-- | The name of the cache engine to be used for this cluster.
--
-- Valid values for this parameter are: @memcached@ | @redis@
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccEngine :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccEngine = Lens.lens (engine :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: CreateCacheCluster)
{-# DEPRECATED cccEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A list of the Availability Zones in which cache nodes are created. The order of the zones in the list is not important.
--
-- This option is only supported on Memcached.
-- If you want all the nodes in the same Availability Zone, use @PreferredAvailabilityZone@ instead, or repeat the Availability Zone multiple times in the list.
-- Default: System chosen Availability Zones.
--
-- /Note:/ Consider using 'preferredAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredAvailabilityZones :: Lens.Lens' CreateCacheCluster (Lude.Maybe [Lude.Text])
cccPreferredAvailabilityZones = Lens.lens (preferredAvailabilityZones :: CreateCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {preferredAvailabilityZones = a} :: CreateCacheCluster)
{-# DEPRECATED cccPreferredAvailabilityZones "Use generic-lens or generic-optics with 'preferredAvailabilityZones' instead." #-}

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
cccPreferredMaintenanceWindow :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CreateCacheCluster)
{-# DEPRECATED cccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The name of the subnet group to be used for the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
-- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCacheSubnetGroupName :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: CreateCacheCluster)
{-# DEPRECATED cccCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | The EC2 Availability Zone in which the cluster is created.
--
-- All nodes belonging to this cluster are placed in the preferred Availability Zone. If you want to create your nodes across multiple Availability Zones, use @PreferredAvailabilityZones@ .
-- Default: System chosen Availability Zone.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredAvailabilityZone :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccPreferredAvailabilityZone = Lens.lens (preferredAvailabilityZone :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredAvailabilityZone = a} :: CreateCacheCluster)
{-# DEPRECATED cccPreferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead." #-}

-- | The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot taken today is retained for 5 days before being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache cluster).
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotRetentionLimit :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Int)
cccSnapshotRetentionLimit = Lens.lens (snapshotRetentionLimit :: CreateCacheCluster -> Lude.Maybe Lude.Int) (\s a -> s {snapshotRetentionLimit = a} :: CreateCacheCluster)
{-# DEPRECATED cccSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The outpost ARNs in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredOutpostARNs :: Lens.Lens' CreateCacheCluster (Lude.Maybe [Lude.Text])
cccPreferredOutpostARNs = Lens.lens (preferredOutpostARNs :: CreateCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {preferredOutpostARNs = a} :: CreateCacheCluster)
{-# DEPRECATED cccPreferredOutpostARNs "Use generic-lens or generic-optics with 'preferredOutpostARNs' instead." #-}

-- | Specifies whether the nodes in this Memcached cluster are created in a single Availability Zone or created across multiple Availability Zones in the cluster's region.
--
-- This parameter is only supported for Memcached clusters.
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified, ElastiCache assumes @single-az@ mode.
--
-- /Note:/ Consider using 'aZMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccAZMode :: Lens.Lens' CreateCacheCluster (Lude.Maybe AZMode)
cccAZMode = Lens.lens (aZMode :: CreateCacheCluster -> Lude.Maybe AZMode) (\s a -> s {aZMode = a} :: CreateCacheCluster)
{-# DEPRECATED cccAZMode "Use generic-lens or generic-optics with 'aZMode' instead." #-}

-- | The name of a Redis snapshot from which to restore data into the new node group (shard). The snapshot status changes to @restoring@ while the new node group (shard) is being created.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotName :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccSnapshotName = Lens.lens (snapshotName :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {snapshotName = a} :: CreateCacheCluster)
{-# DEPRECATED cccSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

-- | The outpost ARN in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredOutpostARN :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccPreferredOutpostARN = Lens.lens (preferredOutpostARN :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredOutpostARN = a} :: CreateCacheCluster)
{-# DEPRECATED cccPreferredOutpostARN "Use generic-lens or generic-optics with 'preferredOutpostARN' instead." #-}

-- | The ID of the replication group to which this cluster should belong. If this parameter is specified, the cluster is added to the specified replication group as a read replica; otherwise, the cluster is a standalone primary that is not part of any replication group.
--
-- If the specified replication group is Multi-AZ enabled and the Availability Zone is not specified, the cluster is created in Availability Zones that provide the best spread of read replicas across Availability Zones.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccReplicationGroupId :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccReplicationGroupId = Lens.lens (replicationGroupId :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: CreateCacheCluster)
{-# DEPRECATED cccReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccNotificationTopicARN :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Text)
cccNotificationTopicARN = Lens.lens (notificationTopicARN :: CreateCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicARN = a} :: CreateCacheCluster)
{-# DEPRECATED cccNotificationTopicARN "Use generic-lens or generic-optics with 'notificationTopicARN' instead." #-}

-- | The initial number of cache nodes that the cluster has.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
-- If you need more than 20 nodes for your Memcached cluster, please fill out the ElastiCache Limit Increase Request form at <http://aws.amazon.com/contact-us/elasticache-node-limit-request/ http://aws.amazon.com/contact-us/elasticache-node-limit-request/> .
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccNumCacheNodes :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Int)
cccNumCacheNodes = Lens.lens (numCacheNodes :: CreateCacheCluster -> Lude.Maybe Lude.Int) (\s a -> s {numCacheNodes = a} :: CreateCacheCluster)
{-# DEPRECATED cccNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

-- | A list of cost allocation tags to be added to this resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccTags :: Lens.Lens' CreateCacheCluster (Lude.Maybe [Tag])
cccTags = Lens.lens (tags :: CreateCacheCluster -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCacheCluster)
{-# DEPRECATED cccTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number on which each of the cache nodes accepts connections.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPort :: Lens.Lens' CreateCacheCluster (Lude.Maybe Lude.Int)
cccPort = Lens.lens (port :: CreateCacheCluster -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CreateCacheCluster)
{-# DEPRECATED cccPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A list of security group names to associate with this cluster.
--
-- Use this parameter only when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCacheSecurityGroupNames :: Lens.Lens' CreateCacheCluster (Lude.Maybe [Lude.Text])
cccCacheSecurityGroupNames = Lens.lens (cacheSecurityGroupNames :: CreateCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheSecurityGroupNames = a} :: CreateCacheCluster)
{-# DEPRECATED cccCacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead." #-}

-- | The node group (shard) identifier. This parameter is stored as a lowercase string.
--
-- __Constraints:__
--
--     * A name must contain from 1 to 50 alphanumeric characters or hyphens.
--
--
--     * The first character must be a letter.
--
--
--     * A name cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCacheClusterId :: Lens.Lens' CreateCacheCluster Lude.Text
cccCacheClusterId = Lens.lens (cacheClusterId :: CreateCacheCluster -> Lude.Text) (\s a -> s {cacheClusterId = a} :: CreateCacheCluster)
{-# DEPRECATED cccCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

instance Lude.AWSRequest CreateCacheCluster where
  type Rs CreateCacheCluster = CreateCacheClusterResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "CreateCacheClusterResult"
      ( \s h x ->
          CreateCacheClusterResponse'
            Lude.<$> (x Lude..@? "CacheCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCacheCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateCacheCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCacheCluster where
  toQuery CreateCacheCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateCacheCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "CacheNodeType" Lude.=: cacheNodeType,
        "SecurityGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        "SnapshotArns"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "SnapshotArn" Lude.<$> snapshotARNs),
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "OutpostMode" Lude.=: outpostMode,
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "SnapshotWindow" Lude.=: snapshotWindow,
        "AuthToken" Lude.=: authToken,
        "Engine" Lude.=: engine,
        "PreferredAvailabilityZones"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "PreferredAvailabilityZone"
                Lude.<$> preferredAvailabilityZones
            ),
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "CacheSubnetGroupName" Lude.=: cacheSubnetGroupName,
        "PreferredAvailabilityZone" Lude.=: preferredAvailabilityZone,
        "SnapshotRetentionLimit" Lude.=: snapshotRetentionLimit,
        "PreferredOutpostArns"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "PreferredOutpostArn"
                Lude.<$> preferredOutpostARNs
            ),
        "AZMode" Lude.=: aZMode,
        "SnapshotName" Lude.=: snapshotName,
        "PreferredOutpostArn" Lude.=: preferredOutpostARN,
        "ReplicationGroupId" Lude.=: replicationGroupId,
        "NotificationTopicArn" Lude.=: notificationTopicARN,
        "NumCacheNodes" Lude.=: numCacheNodes,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "CacheSecurityGroupNames"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "CacheSecurityGroupName"
                Lude.<$> cacheSecurityGroupNames
            ),
        "CacheClusterId" Lude.=: cacheClusterId
      ]

-- | /See:/ 'mkCreateCacheClusterResponse' smart constructor.
data CreateCacheClusterResponse = CreateCacheClusterResponse'
  { cacheCluster ::
      Lude.Maybe CacheCluster,
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

-- | Creates a value of 'CreateCacheClusterResponse' with the minimum fields required to make a request.
--
-- * 'cacheCluster' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkCreateCacheClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCacheClusterResponse
mkCreateCacheClusterResponse pResponseStatus_ =
  CreateCacheClusterResponse'
    { cacheCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrsCacheCluster :: Lens.Lens' CreateCacheClusterResponse (Lude.Maybe CacheCluster)
cccrsCacheCluster = Lens.lens (cacheCluster :: CreateCacheClusterResponse -> Lude.Maybe CacheCluster) (\s a -> s {cacheCluster = a} :: CreateCacheClusterResponse)
{-# DEPRECATED cccrsCacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrsResponseStatus :: Lens.Lens' CreateCacheClusterResponse Lude.Int
cccrsResponseStatus = Lens.lens (responseStatus :: CreateCacheClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCacheClusterResponse)
{-# DEPRECATED cccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
