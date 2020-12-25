{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheCluster
  ( CacheCluster (..),

    -- * Smart constructor
    mkCacheCluster,

    -- * Lenses
    ccARN,
    ccAtRestEncryptionEnabled,
    ccAuthTokenEnabled,
    ccAuthTokenLastModifiedDate,
    ccAutoMinorVersionUpgrade,
    ccCacheClusterCreateTime,
    ccCacheClusterId,
    ccCacheClusterStatus,
    ccCacheNodeType,
    ccCacheNodes,
    ccCacheParameterGroup,
    ccCacheSecurityGroups,
    ccCacheSubnetGroupName,
    ccClientDownloadLandingPage,
    ccConfigurationEndpoint,
    ccEngine,
    ccEngineVersion,
    ccNotificationConfiguration,
    ccNumCacheNodes,
    ccPendingModifiedValues,
    ccPreferredAvailabilityZone,
    ccPreferredMaintenanceWindow,
    ccPreferredOutpostArn,
    ccReplicationGroupId,
    ccSecurityGroups,
    ccSnapshotRetentionLimit,
    ccSnapshotWindow,
    ccTransitEncryptionEnabled,
  )
where

import qualified Network.AWS.ElastiCache.Types.CacheNode as Types
import qualified Network.AWS.ElastiCache.Types.CacheParameterGroupStatus as Types
import qualified Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership as Types
import qualified Network.AWS.ElastiCache.Types.Endpoint as Types
import qualified Network.AWS.ElastiCache.Types.NotificationConfiguration as Types
import qualified Network.AWS.ElastiCache.Types.PendingModifiedValues as Types
import qualified Network.AWS.ElastiCache.Types.SecurityGroupMembership as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains all of the attributes of a specific cluster.
--
-- /See:/ 'mkCacheCluster' smart constructor.
data CacheCluster = CacheCluster'
  { -- | The ARN (Amazon Resource Name) of the cache cluster.
    arn :: Core.Maybe Types.String,
    -- | A flag that enables encryption at-rest when set to @true@ .
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
    -- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
    -- Default: @false@
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
    --
    -- Default: @false@
    authTokenEnabled :: Core.Maybe Core.Bool,
    -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Core.Maybe Core.UTCTime,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The date and time when the cluster was created.
    cacheClusterCreateTime :: Core.Maybe Core.UTCTime,
    -- | The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
    cacheClusterId :: Core.Maybe Types.String,
    -- | The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
    cacheClusterStatus :: Core.Maybe Types.String,
    -- | The name of the compute and memory capacity node type for the cluster.
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
    cacheNodeType :: Core.Maybe Types.String,
    -- | A list of cache nodes that are members of the cluster.
    cacheNodes :: Core.Maybe [Types.CacheNode],
    -- | Status of the cache parameter group.
    cacheParameterGroup :: Core.Maybe Types.CacheParameterGroupStatus,
    -- | A list of cache security group elements, composed of name and status sub-elements.
    cacheSecurityGroups :: Core.Maybe [Types.CacheSecurityGroupMembership],
    -- | The name of the cache subnet group associated with the cluster.
    cacheSubnetGroupName :: Core.Maybe Types.String,
    -- | The URL of the web page where you can download the latest ElastiCache client library.
    clientDownloadLandingPage :: Core.Maybe Types.String,
    -- | Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it.
    --
    -- Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@
    configurationEndpoint :: Core.Maybe Types.Endpoint,
    -- | The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
    engine :: Core.Maybe Types.String,
    -- | The version of the cache engine that is used in this cluster.
    engineVersion :: Core.Maybe Types.String,
    -- | Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
    notificationConfiguration :: Core.Maybe Types.NotificationConfiguration,
    -- | The number of cache nodes in the cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
    numCacheNodes :: Core.Maybe Core.Int,
    pendingModifiedValues :: Core.Maybe Types.PendingModifiedValues,
    -- | The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
    preferredAvailabilityZone :: Core.Maybe Types.String,
    -- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
    --
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
    preferredMaintenanceWindow :: Core.Maybe Types.String,
    -- | The outpost ARN in which the cache cluster is created.
    preferredOutpostArn :: Core.Maybe Types.String,
    -- | The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
    replicationGroupId :: Core.Maybe Types.String,
    -- | A list of VPC Security Groups associated with the cluster.
    securityGroups :: Core.Maybe [Types.SecurityGroupMembership],
    -- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
    --
    -- /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
    snapshotRetentionLimit :: Core.Maybe Core.Int,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster.
    --
    -- Example: @05:00-09:00@
    snapshotWindow :: Core.Maybe Types.String,
    -- | A flag that enables in-transit encryption when set to @true@ .
    --
    -- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
    -- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
    -- Default: @false@
    transitEncryptionEnabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CacheCluster' value with any optional fields omitted.
mkCacheCluster ::
  CacheCluster
mkCacheCluster =
  CacheCluster'
    { arn = Core.Nothing,
      atRestEncryptionEnabled = Core.Nothing,
      authTokenEnabled = Core.Nothing,
      authTokenLastModifiedDate = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      cacheClusterCreateTime = Core.Nothing,
      cacheClusterId = Core.Nothing,
      cacheClusterStatus = Core.Nothing,
      cacheNodeType = Core.Nothing,
      cacheNodes = Core.Nothing,
      cacheParameterGroup = Core.Nothing,
      cacheSecurityGroups = Core.Nothing,
      cacheSubnetGroupName = Core.Nothing,
      clientDownloadLandingPage = Core.Nothing,
      configurationEndpoint = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      notificationConfiguration = Core.Nothing,
      numCacheNodes = Core.Nothing,
      pendingModifiedValues = Core.Nothing,
      preferredAvailabilityZone = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      preferredOutpostArn = Core.Nothing,
      replicationGroupId = Core.Nothing,
      securityGroups = Core.Nothing,
      snapshotRetentionLimit = Core.Nothing,
      snapshotWindow = Core.Nothing,
      transitEncryptionEnabled = Core.Nothing
    }

-- | The ARN (Amazon Resource Name) of the cache cluster.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccARN :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccARN = Lens.field @"arn"
{-# DEPRECATED ccARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAtRestEncryptionEnabled :: Lens.Lens' CacheCluster (Core.Maybe Core.Bool)
ccAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# DEPRECATED ccAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthTokenEnabled :: Lens.Lens' CacheCluster (Core.Maybe Core.Bool)
ccAuthTokenEnabled = Lens.field @"authTokenEnabled"
{-# DEPRECATED ccAuthTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead." #-}

-- | The date the auth token was last modified
--
-- /Note:/ Consider using 'authTokenLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthTokenLastModifiedDate :: Lens.Lens' CacheCluster (Core.Maybe Core.UTCTime)
ccAuthTokenLastModifiedDate = Lens.field @"authTokenLastModifiedDate"
{-# DEPRECATED ccAuthTokenLastModifiedDate "Use generic-lens or generic-optics with 'authTokenLastModifiedDate' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAutoMinorVersionUpgrade :: Lens.Lens' CacheCluster (Core.Maybe Core.Bool)
ccAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED ccAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The date and time when the cluster was created.
--
-- /Note:/ Consider using 'cacheClusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheClusterCreateTime :: Lens.Lens' CacheCluster (Core.Maybe Core.UTCTime)
ccCacheClusterCreateTime = Lens.field @"cacheClusterCreateTime"
{-# DEPRECATED ccCacheClusterCreateTime "Use generic-lens or generic-optics with 'cacheClusterCreateTime' instead." #-}

-- | The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheClusterId :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED ccCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
--
-- /Note:/ Consider using 'cacheClusterStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheClusterStatus :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccCacheClusterStatus = Lens.field @"cacheClusterStatus"
{-# DEPRECATED ccCacheClusterStatus "Use generic-lens or generic-optics with 'cacheClusterStatus' instead." #-}

-- | The name of the compute and memory capacity node type for the cluster.
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
ccCacheNodeType :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED ccCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | A list of cache nodes that are members of the cluster.
--
-- /Note:/ Consider using 'cacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheNodes :: Lens.Lens' CacheCluster (Core.Maybe [Types.CacheNode])
ccCacheNodes = Lens.field @"cacheNodes"
{-# DEPRECATED ccCacheNodes "Use generic-lens or generic-optics with 'cacheNodes' instead." #-}

-- | Status of the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheParameterGroup :: Lens.Lens' CacheCluster (Core.Maybe Types.CacheParameterGroupStatus)
ccCacheParameterGroup = Lens.field @"cacheParameterGroup"
{-# DEPRECATED ccCacheParameterGroup "Use generic-lens or generic-optics with 'cacheParameterGroup' instead." #-}

-- | A list of cache security group elements, composed of name and status sub-elements.
--
-- /Note:/ Consider using 'cacheSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheSecurityGroups :: Lens.Lens' CacheCluster (Core.Maybe [Types.CacheSecurityGroupMembership])
ccCacheSecurityGroups = Lens.field @"cacheSecurityGroups"
{-# DEPRECATED ccCacheSecurityGroups "Use generic-lens or generic-optics with 'cacheSecurityGroups' instead." #-}

-- | The name of the cache subnet group associated with the cluster.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheSubnetGroupName :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# DEPRECATED ccCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | The URL of the web page where you can download the latest ElastiCache client library.
--
-- /Note:/ Consider using 'clientDownloadLandingPage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientDownloadLandingPage :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccClientDownloadLandingPage = Lens.field @"clientDownloadLandingPage"
{-# DEPRECATED ccClientDownloadLandingPage "Use generic-lens or generic-optics with 'clientDownloadLandingPage' instead." #-}

-- | Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it.
--
-- Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@
--
-- /Note:/ Consider using 'configurationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConfigurationEndpoint :: Lens.Lens' CacheCluster (Core.Maybe Types.Endpoint)
ccConfigurationEndpoint = Lens.field @"configurationEndpoint"
{-# DEPRECATED ccConfigurationEndpoint "Use generic-lens or generic-optics with 'configurationEndpoint' instead." #-}

-- | The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngine :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccEngine = Lens.field @"engine"
{-# DEPRECATED ccEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The version of the cache engine that is used in this cluster.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngineVersion :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED ccEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotificationConfiguration :: Lens.Lens' CacheCluster (Core.Maybe Types.NotificationConfiguration)
ccNotificationConfiguration = Lens.field @"notificationConfiguration"
{-# DEPRECATED ccNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | The number of cache nodes in the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNumCacheNodes :: Lens.Lens' CacheCluster (Core.Maybe Core.Int)
ccNumCacheNodes = Lens.field @"numCacheNodes"
{-# DEPRECATED ccNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPendingModifiedValues :: Lens.Lens' CacheCluster (Core.Maybe Types.PendingModifiedValues)
ccPendingModifiedValues = Lens.field @"pendingModifiedValues"
{-# DEPRECATED ccPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredAvailabilityZone :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccPreferredAvailabilityZone = Lens.field @"preferredAvailabilityZone"
{-# DEPRECATED ccPreferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead." #-}

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
--
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
ccPreferredMaintenanceWindow :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED ccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The outpost ARN in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredOutpostArn :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccPreferredOutpostArn = Lens.field @"preferredOutpostArn"
{-# DEPRECATED ccPreferredOutpostArn "Use generic-lens or generic-optics with 'preferredOutpostArn' instead." #-}

-- | The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReplicationGroupId :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED ccReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | A list of VPC Security Groups associated with the cluster.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSecurityGroups :: Lens.Lens' CacheCluster (Core.Maybe [Types.SecurityGroupMembership])
ccSecurityGroups = Lens.field @"securityGroups"
{-# DEPRECATED ccSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnapshotRetentionLimit :: Lens.Lens' CacheCluster (Core.Maybe Core.Int)
ccSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# DEPRECATED ccSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster.
--
-- Example: @05:00-09:00@
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnapshotWindow :: Lens.Lens' CacheCluster (Core.Maybe Types.String)
ccSnapshotWindow = Lens.field @"snapshotWindow"
{-# DEPRECATED ccSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTransitEncryptionEnabled :: Lens.Lens' CacheCluster (Core.Maybe Core.Bool)
ccTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# DEPRECATED ccTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

instance Core.FromXML CacheCluster where
  parseXML x =
    CacheCluster'
      Core.<$> (x Core..@? "ARN")
      Core.<*> (x Core..@? "AtRestEncryptionEnabled")
      Core.<*> (x Core..@? "AuthTokenEnabled")
      Core.<*> (x Core..@? "AuthTokenLastModifiedDate")
      Core.<*> (x Core..@? "AutoMinorVersionUpgrade")
      Core.<*> (x Core..@? "CacheClusterCreateTime")
      Core.<*> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "CacheClusterStatus")
      Core.<*> (x Core..@? "CacheNodeType")
      Core.<*> (x Core..@? "CacheNodes" Core..<@> Core.parseXMLList "CacheNode")
      Core.<*> (x Core..@? "CacheParameterGroup")
      Core.<*> ( x Core..@? "CacheSecurityGroups"
                   Core..<@> Core.parseXMLList "CacheSecurityGroup"
               )
      Core.<*> (x Core..@? "CacheSubnetGroupName")
      Core.<*> (x Core..@? "ClientDownloadLandingPage")
      Core.<*> (x Core..@? "ConfigurationEndpoint")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "NotificationConfiguration")
      Core.<*> (x Core..@? "NumCacheNodes")
      Core.<*> (x Core..@? "PendingModifiedValues")
      Core.<*> (x Core..@? "PreferredAvailabilityZone")
      Core.<*> (x Core..@? "PreferredMaintenanceWindow")
      Core.<*> (x Core..@? "PreferredOutpostArn")
      Core.<*> (x Core..@? "ReplicationGroupId")
      Core.<*> (x Core..@? "SecurityGroups" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "SnapshotRetentionLimit")
      Core.<*> (x Core..@? "SnapshotWindow")
      Core.<*> (x Core..@? "TransitEncryptionEnabled")
