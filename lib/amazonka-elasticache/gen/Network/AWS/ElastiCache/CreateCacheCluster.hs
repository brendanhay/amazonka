{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateCacheCluster (..)
    , mkCreateCacheCluster
    -- ** Request lenses
    , cccCacheClusterId
    , cccAZMode
    , cccAuthToken
    , cccAutoMinorVersionUpgrade
    , cccCacheNodeType
    , cccCacheParameterGroupName
    , cccCacheSecurityGroupNames
    , cccCacheSubnetGroupName
    , cccEngine
    , cccEngineVersion
    , cccNotificationTopicArn
    , cccNumCacheNodes
    , cccOutpostMode
    , cccPort
    , cccPreferredAvailabilityZone
    , cccPreferredAvailabilityZones
    , cccPreferredMaintenanceWindow
    , cccPreferredOutpostArn
    , cccPreferredOutpostArns
    , cccReplicationGroupId
    , cccSecurityGroupIds
    , cccSnapshotArns
    , cccSnapshotName
    , cccSnapshotRetentionLimit
    , cccSnapshotWindow
    , cccTags

    -- * Destructuring the response
    , CreateCacheClusterResponse (..)
    , mkCreateCacheClusterResponse
    -- ** Response lenses
    , cccrrsCacheCluster
    , cccrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a CreateCacheCluster operation.
--
-- /See:/ 'mkCreateCacheCluster' smart constructor.
data CreateCacheCluster = CreateCacheCluster'
  { cacheClusterId :: Core.Text
    -- ^ The node group (shard) identifier. This parameter is stored as a lowercase string.
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
  , aZMode :: Core.Maybe Types.AZMode
    -- ^ Specifies whether the nodes in this Memcached cluster are created in a single Availability Zone or created across multiple Availability Zones in the cluster's region.
--
-- This parameter is only supported for Memcached clusters.
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified, ElastiCache assumes @single-az@ mode.
  , authToken :: Core.Maybe Core.Text
    -- ^ __Reserved parameter.__ The password used to access a password protected server.
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
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ This parameter is currently disabled.
  , cacheNodeType :: Core.Maybe Core.Text
    -- ^ The compute and memory capacity of the nodes in the node group (shard).
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
  , cacheParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the parameter group to associate with this cluster. If this argument is omitted, the default parameter group for the specified engine is used. You cannot use any parameter group which has @cluster-enabled='yes'@ when creating a cluster.
  , cacheSecurityGroupNames :: Core.Maybe [Core.Text]
    -- ^ A list of security group names to associate with this cluster.
--
-- Use this parameter only when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC).
  , cacheSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the subnet group to be used for the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
-- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
  , engine :: Core.Maybe Core.Text
    -- ^ The name of the cache engine to be used for this cluster.
--
-- Valid values for this parameter are: @memcached@ | @redis@ 
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The version number of the cache engine to be used for this cluster. To view the supported cache engine versions, use the DescribeCacheEngineVersions operation.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version. 
  , notificationTopicArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
  , numCacheNodes :: Core.Maybe Core.Int
    -- ^ The initial number of cache nodes that the cluster has.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
-- If you need more than 20 nodes for your Memcached cluster, please fill out the ElastiCache Limit Increase Request form at <http://aws.amazon.com/contact-us/elasticache-node-limit-request/ http://aws.amazon.com/contact-us/elasticache-node-limit-request/> .
  , outpostMode :: Core.Maybe Types.OutpostMode
    -- ^ Specifies whether the nodes in the cluster are created in a single outpost or across multiple outposts.
  , port :: Core.Maybe Core.Int
    -- ^ The port number on which each of the cache nodes accepts connections.
  , preferredAvailabilityZone :: Core.Maybe Core.Text
    -- ^ The EC2 Availability Zone in which the cluster is created.
--
-- All nodes belonging to this cluster are placed in the preferred Availability Zone. If you want to create your nodes across multiple Availability Zones, use @PreferredAvailabilityZones@ .
-- Default: System chosen Availability Zone.
  , preferredAvailabilityZones :: Core.Maybe [Core.Text]
    -- ^ A list of the Availability Zones in which cache nodes are created. The order of the zones in the list is not important.
--
-- This option is only supported on Memcached.
-- If you want all the nodes in the same Availability Zone, use @PreferredAvailabilityZone@ instead, or repeat the Availability Zone multiple times in the list.
-- Default: System chosen Availability Zones.
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:
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
  , preferredOutpostArn :: Core.Maybe Core.Text
    -- ^ The outpost ARN in which the cache cluster is created.
  , preferredOutpostArns :: Core.Maybe [Core.Text]
    -- ^ The outpost ARNs in which the cache cluster is created.
  , replicationGroupId :: Core.Maybe Core.Text
    -- ^ The ID of the replication group to which this cluster should belong. If this parameter is specified, the cluster is added to the specified replication group as a read replica; otherwise, the cluster is a standalone primary that is not part of any replication group.
--
-- If the specified replication group is Multi-AZ enabled and the Availability Zone is not specified, the cluster is created in Availability Zones that provide the best spread of read replicas across Availability Zones.
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ One or more VPC security groups associated with the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
  , snapshotArns :: Core.Maybe [Core.Text]
    -- ^ A single-element string list containing an Amazon Resource Name (ARN) that uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The snapshot file is used to populate the node group (shard). The Amazon S3 object name in the ARN cannot contain any commas.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@ 
  , snapshotName :: Core.Maybe Core.Text
    -- ^ The name of a Redis snapshot from which to restore data into the new node group (shard). The snapshot status changes to @restoring@ while the new node group (shard) is being created.
  , snapshotRetentionLimit :: Core.Maybe Core.Int
    -- ^ The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot taken today is retained for 5 days before being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache cluster).
  , snapshotWindow :: Core.Maybe Core.Text
    -- ^ The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@ 
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of cost allocation tags to be added to this resource.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCacheCluster' value with any optional fields omitted.
mkCreateCacheCluster
    :: Core.Text -- ^ 'cacheClusterId'
    -> CreateCacheCluster
mkCreateCacheCluster cacheClusterId
  = CreateCacheCluster'{cacheClusterId, aZMode = Core.Nothing,
                        authToken = Core.Nothing, autoMinorVersionUpgrade = Core.Nothing,
                        cacheNodeType = Core.Nothing,
                        cacheParameterGroupName = Core.Nothing,
                        cacheSecurityGroupNames = Core.Nothing,
                        cacheSubnetGroupName = Core.Nothing, engine = Core.Nothing,
                        engineVersion = Core.Nothing, notificationTopicArn = Core.Nothing,
                        numCacheNodes = Core.Nothing, outpostMode = Core.Nothing,
                        port = Core.Nothing, preferredAvailabilityZone = Core.Nothing,
                        preferredAvailabilityZones = Core.Nothing,
                        preferredMaintenanceWindow = Core.Nothing,
                        preferredOutpostArn = Core.Nothing,
                        preferredOutpostArns = Core.Nothing,
                        replicationGroupId = Core.Nothing, securityGroupIds = Core.Nothing,
                        snapshotArns = Core.Nothing, snapshotName = Core.Nothing,
                        snapshotRetentionLimit = Core.Nothing,
                        snapshotWindow = Core.Nothing, tags = Core.Nothing}

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
cccCacheClusterId :: Lens.Lens' CreateCacheCluster Core.Text
cccCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE cccCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | Specifies whether the nodes in this Memcached cluster are created in a single Availability Zone or created across multiple Availability Zones in the cluster's region.
--
-- This parameter is only supported for Memcached clusters.
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified, ElastiCache assumes @single-az@ mode.
--
-- /Note:/ Consider using 'aZMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccAZMode :: Lens.Lens' CreateCacheCluster (Core.Maybe Types.AZMode)
cccAZMode = Lens.field @"aZMode"
{-# INLINEABLE cccAZMode #-}
{-# DEPRECATED aZMode "Use generic-lens or generic-optics with 'aZMode' instead"  #-}

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
cccAuthToken :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccAuthToken = Lens.field @"authToken"
{-# INLINEABLE cccAuthToken #-}
{-# DEPRECATED authToken "Use generic-lens or generic-optics with 'authToken' instead"  #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccAutoMinorVersionUpgrade :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Bool)
cccAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE cccAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

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
cccCacheNodeType :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE cccCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | The name of the parameter group to associate with this cluster. If this argument is omitted, the default parameter group for the specified engine is used. You cannot use any parameter group which has @cluster-enabled='yes'@ when creating a cluster.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCacheParameterGroupName :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE cccCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | A list of security group names to associate with this cluster.
--
-- Use this parameter only when you are creating a cluster outside of an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCacheSecurityGroupNames :: Lens.Lens' CreateCacheCluster (Core.Maybe [Core.Text])
cccCacheSecurityGroupNames = Lens.field @"cacheSecurityGroupNames"
{-# INLINEABLE cccCacheSecurityGroupNames #-}
{-# DEPRECATED cacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead"  #-}

-- | The name of the subnet group to be used for the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
-- /Important:/ If you're going to launch your cluster in an Amazon VPC, you need to create a subnet group before you start creating a cluster. For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups> .
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCacheSubnetGroupName :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# INLINEABLE cccCacheSubnetGroupName #-}
{-# DEPRECATED cacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead"  #-}

-- | The name of the cache engine to be used for this cluster.
--
-- Valid values for this parameter are: @memcached@ | @redis@ 
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccEngine :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccEngine = Lens.field @"engine"
{-# INLINEABLE cccEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The version number of the cache engine to be used for this cluster. To view the supported cache engine versions, use the DescribeCacheEngineVersions operation.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster or replication group and create it anew with the earlier engine version. 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccEngineVersion :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE cccEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service (SNS) topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccNotificationTopicArn :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccNotificationTopicArn = Lens.field @"notificationTopicArn"
{-# INLINEABLE cccNotificationTopicArn #-}
{-# DEPRECATED notificationTopicArn "Use generic-lens or generic-optics with 'notificationTopicArn' instead"  #-}

-- | The initial number of cache nodes that the cluster has.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
-- If you need more than 20 nodes for your Memcached cluster, please fill out the ElastiCache Limit Increase Request form at <http://aws.amazon.com/contact-us/elasticache-node-limit-request/ http://aws.amazon.com/contact-us/elasticache-node-limit-request/> .
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccNumCacheNodes :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Int)
cccNumCacheNodes = Lens.field @"numCacheNodes"
{-# INLINEABLE cccNumCacheNodes #-}
{-# DEPRECATED numCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead"  #-}

-- | Specifies whether the nodes in the cluster are created in a single outpost or across multiple outposts.
--
-- /Note:/ Consider using 'outpostMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccOutpostMode :: Lens.Lens' CreateCacheCluster (Core.Maybe Types.OutpostMode)
cccOutpostMode = Lens.field @"outpostMode"
{-# INLINEABLE cccOutpostMode #-}
{-# DEPRECATED outpostMode "Use generic-lens or generic-optics with 'outpostMode' instead"  #-}

-- | The port number on which each of the cache nodes accepts connections.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPort :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Int)
cccPort = Lens.field @"port"
{-# INLINEABLE cccPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The EC2 Availability Zone in which the cluster is created.
--
-- All nodes belonging to this cluster are placed in the preferred Availability Zone. If you want to create your nodes across multiple Availability Zones, use @PreferredAvailabilityZones@ .
-- Default: System chosen Availability Zone.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredAvailabilityZone :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccPreferredAvailabilityZone = Lens.field @"preferredAvailabilityZone"
{-# INLINEABLE cccPreferredAvailabilityZone #-}
{-# DEPRECATED preferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead"  #-}

-- | A list of the Availability Zones in which cache nodes are created. The order of the zones in the list is not important.
--
-- This option is only supported on Memcached.
-- If you want all the nodes in the same Availability Zone, use @PreferredAvailabilityZone@ instead, or repeat the Availability Zone multiple times in the list.
-- Default: System chosen Availability Zones.
--
-- /Note:/ Consider using 'preferredAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredAvailabilityZones :: Lens.Lens' CreateCacheCluster (Core.Maybe [Core.Text])
cccPreferredAvailabilityZones = Lens.field @"preferredAvailabilityZones"
{-# INLINEABLE cccPreferredAvailabilityZones #-}
{-# DEPRECATED preferredAvailabilityZones "Use generic-lens or generic-optics with 'preferredAvailabilityZones' instead"  #-}

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
cccPreferredMaintenanceWindow :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE cccPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | The outpost ARN in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredOutpostArn :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccPreferredOutpostArn = Lens.field @"preferredOutpostArn"
{-# INLINEABLE cccPreferredOutpostArn #-}
{-# DEPRECATED preferredOutpostArn "Use generic-lens or generic-optics with 'preferredOutpostArn' instead"  #-}

-- | The outpost ARNs in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccPreferredOutpostArns :: Lens.Lens' CreateCacheCluster (Core.Maybe [Core.Text])
cccPreferredOutpostArns = Lens.field @"preferredOutpostArns"
{-# INLINEABLE cccPreferredOutpostArns #-}
{-# DEPRECATED preferredOutpostArns "Use generic-lens or generic-optics with 'preferredOutpostArns' instead"  #-}

-- | The ID of the replication group to which this cluster should belong. If this parameter is specified, the cluster is added to the specified replication group as a read replica; otherwise, the cluster is a standalone primary that is not part of any replication group.
--
-- If the specified replication group is Multi-AZ enabled and the Availability Zone is not specified, the cluster is created in Availability Zones that provide the best spread of read replicas across Availability Zones.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccReplicationGroupId :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccReplicationGroupId = Lens.field @"replicationGroupId"
{-# INLINEABLE cccReplicationGroupId #-}
{-# DEPRECATED replicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead"  #-}

-- | One or more VPC security groups associated with the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSecurityGroupIds :: Lens.Lens' CreateCacheCluster (Core.Maybe [Core.Text])
cccSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE cccSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | A single-element string list containing an Amazon Resource Name (ARN) that uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The snapshot file is used to populate the node group (shard). The Amazon S3 object name in the ARN cannot contain any commas.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket/snapshot1.rdb@ 
--
-- /Note:/ Consider using 'snapshotArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotArns :: Lens.Lens' CreateCacheCluster (Core.Maybe [Core.Text])
cccSnapshotArns = Lens.field @"snapshotArns"
{-# INLINEABLE cccSnapshotArns #-}
{-# DEPRECATED snapshotArns "Use generic-lens or generic-optics with 'snapshotArns' instead"  #-}

-- | The name of a Redis snapshot from which to restore data into the new node group (shard). The snapshot status changes to @restoring@ while the new node group (shard) is being created.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotName :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccSnapshotName = Lens.field @"snapshotName"
{-# INLINEABLE cccSnapshotName #-}
{-# DEPRECATED snapshotName "Use generic-lens or generic-optics with 'snapshotName' instead"  #-}

-- | The number of days for which ElastiCache retains automatic snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot taken today is retained for 5 days before being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache cluster).
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotRetentionLimit :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Int)
cccSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# INLINEABLE cccSnapshotRetentionLimit #-}
{-# DEPRECATED snapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead"  #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@ 
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccSnapshotWindow :: Lens.Lens' CreateCacheCluster (Core.Maybe Core.Text)
cccSnapshotWindow = Lens.field @"snapshotWindow"
{-# INLINEABLE cccSnapshotWindow #-}
{-# DEPRECATED snapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead"  #-}

-- | A list of cost allocation tags to be added to this resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccTags :: Lens.Lens' CreateCacheCluster (Core.Maybe [Types.Tag])
cccTags = Lens.field @"tags"
{-# INLINEABLE cccTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateCacheCluster where
        toQuery CreateCacheCluster{..}
          = Core.toQueryPair "Action" ("CreateCacheCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "CacheClusterId" cacheClusterId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "AZMode") aZMode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AuthToken") authToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoMinorVersionUpgrade")
                autoMinorVersionUpgrade
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheNodeType")
                cacheNodeType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheParameterGroupName")
                cacheParameterGroupName
              Core.<>
              Core.toQueryPair "CacheSecurityGroupNames"
                (Core.maybe Core.mempty (Core.toQueryList "CacheSecurityGroupName")
                   cacheSecurityGroupNames)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CacheSubnetGroupName")
                cacheSubnetGroupName
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Engine") engine
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NotificationTopicArn")
                notificationTopicArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NumCacheNodes")
                numCacheNodes
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OutpostMode") outpostMode
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredAvailabilityZone")
                preferredAvailabilityZone
              Core.<>
              Core.toQueryPair "PreferredAvailabilityZones"
                (Core.maybe Core.mempty
                   (Core.toQueryList "PreferredAvailabilityZone")
                   preferredAvailabilityZones)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredMaintenanceWindow")
                preferredMaintenanceWindow
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PreferredOutpostArn")
                preferredOutpostArn
              Core.<>
              Core.toQueryPair "PreferredOutpostArns"
                (Core.maybe Core.mempty (Core.toQueryList "PreferredOutpostArn")
                   preferredOutpostArns)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplicationGroupId")
                replicationGroupId
              Core.<>
              Core.toQueryPair "SecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId")
                   securityGroupIds)
              Core.<>
              Core.toQueryPair "SnapshotArns"
                (Core.maybe Core.mempty (Core.toQueryList "SnapshotArn")
                   snapshotArns)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotName")
                snapshotName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotRetentionLimit")
                snapshotRetentionLimit
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotWindow")
                snapshotWindow
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateCacheCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateCacheCluster where
        type Rs CreateCacheCluster = CreateCacheClusterResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateCacheClusterResult"
              (\ s h x ->
                 CreateCacheClusterResponse' Core.<$>
                   (x Core..@? "CacheCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateCacheClusterResponse' smart constructor.
data CreateCacheClusterResponse = CreateCacheClusterResponse'
  { cacheCluster :: Core.Maybe Types.CacheCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateCacheClusterResponse' value with any optional fields omitted.
mkCreateCacheClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateCacheClusterResponse
mkCreateCacheClusterResponse responseStatus
  = CreateCacheClusterResponse'{cacheCluster = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrrsCacheCluster :: Lens.Lens' CreateCacheClusterResponse (Core.Maybe Types.CacheCluster)
cccrrsCacheCluster = Lens.field @"cacheCluster"
{-# INLINEABLE cccrrsCacheCluster #-}
{-# DEPRECATED cacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccrrsResponseStatus :: Lens.Lens' CreateCacheClusterResponse Core.Int
cccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
