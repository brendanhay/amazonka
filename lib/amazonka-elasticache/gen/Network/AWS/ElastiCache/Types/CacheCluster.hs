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
    ccAuthTokenLastModifiedDate,
    ccEngineVersion,
    ccCacheNodeType,
    ccCacheNodes,
    ccCacheClusterCreateTime,
    ccAtRestEncryptionEnabled,
    ccAutoMinorVersionUpgrade,
    ccSecurityGroups,
    ccNotificationConfiguration,
    ccARN,
    ccTransitEncryptionEnabled,
    ccSnapshotWindow,
    ccCacheClusterId,
    ccConfigurationEndpoint,
    ccEngine,
    ccCacheSecurityGroups,
    ccAuthTokenEnabled,
    ccClientDownloadLandingPage,
    ccPreferredMaintenanceWindow,
    ccCacheSubnetGroupName,
    ccPreferredAvailabilityZone,
    ccCacheParameterGroup,
    ccCacheClusterStatus,
    ccSnapshotRetentionLimit,
    ccPreferredOutpostARN,
    ccReplicationGroupId,
    ccPendingModifiedValues,
    ccNumCacheNodes,
  )
where

import Network.AWS.ElastiCache.Types.CacheNode
import Network.AWS.ElastiCache.Types.CacheParameterGroupStatus
import Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.NotificationConfiguration
import Network.AWS.ElastiCache.Types.PendingModifiedValues
import Network.AWS.ElastiCache.Types.SecurityGroupMembership
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains all of the attributes of a specific cluster.
--
-- /See:/ 'mkCacheCluster' smart constructor.
data CacheCluster = CacheCluster'
  { authTokenLastModifiedDate ::
      Lude.Maybe Lude.DateTime,
    engineVersion :: Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    cacheNodes :: Lude.Maybe [CacheNode],
    cacheClusterCreateTime :: Lude.Maybe Lude.DateTime,
    atRestEncryptionEnabled :: Lude.Maybe Lude.Bool,
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    securityGroups :: Lude.Maybe [SecurityGroupMembership],
    notificationConfiguration :: Lude.Maybe NotificationConfiguration,
    arn :: Lude.Maybe Lude.Text,
    transitEncryptionEnabled :: Lude.Maybe Lude.Bool,
    snapshotWindow :: Lude.Maybe Lude.Text,
    cacheClusterId :: Lude.Maybe Lude.Text,
    configurationEndpoint :: Lude.Maybe Endpoint,
    engine :: Lude.Maybe Lude.Text,
    cacheSecurityGroups :: Lude.Maybe [CacheSecurityGroupMembership],
    authTokenEnabled :: Lude.Maybe Lude.Bool,
    clientDownloadLandingPage :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    cacheSubnetGroupName :: Lude.Maybe Lude.Text,
    preferredAvailabilityZone :: Lude.Maybe Lude.Text,
    cacheParameterGroup :: Lude.Maybe CacheParameterGroupStatus,
    cacheClusterStatus :: Lude.Maybe Lude.Text,
    snapshotRetentionLimit :: Lude.Maybe Lude.Int,
    preferredOutpostARN :: Lude.Maybe Lude.Text,
    replicationGroupId :: Lude.Maybe Lude.Text,
    pendingModifiedValues :: Lude.Maybe PendingModifiedValues,
    numCacheNodes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheCluster' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon Resource Name) of the cache cluster.
-- * 'atRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
-- * 'authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
-- * 'authTokenLastModifiedDate' - The date the auth token was last modified
-- * 'autoMinorVersionUpgrade' - This parameter is currently disabled.
-- * 'cacheClusterCreateTime' - The date and time when the cluster was created.
-- * 'cacheClusterId' - The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
-- * 'cacheClusterStatus' - The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
-- * 'cacheNodeType' - The name of the compute and memory capacity node type for the cluster.
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
-- * 'cacheNodes' - A list of cache nodes that are members of the cluster.
-- * 'cacheParameterGroup' - Status of the cache parameter group.
-- * 'cacheSecurityGroups' - A list of cache security group elements, composed of name and status sub-elements.
-- * 'cacheSubnetGroupName' - The name of the cache subnet group associated with the cluster.
-- * 'clientDownloadLandingPage' - The URL of the web page where you can download the latest ElastiCache client library.
-- * 'configurationEndpoint' - Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it.
--
-- Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@
-- * 'engine' - The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
-- * 'engineVersion' - The version of the cache engine that is used in this cluster.
-- * 'notificationConfiguration' - Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
-- * 'numCacheNodes' - The number of cache nodes in the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
-- * 'pendingModifiedValues' - Undocumented field.
-- * 'preferredAvailabilityZone' - The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
-- * 'preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
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
-- * 'preferredOutpostARN' - The outpost ARN in which the cache cluster is created.
-- * 'replicationGroupId' - The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
-- * 'securityGroups' - A list of VPC Security Groups associated with the cluster.
-- * 'snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
-- * 'snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster.
--
-- Example: @05:00-09:00@
-- * 'transitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
mkCacheCluster ::
  CacheCluster
mkCacheCluster =
  CacheCluster'
    { authTokenLastModifiedDate = Lude.Nothing,
      engineVersion = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      cacheNodes = Lude.Nothing,
      cacheClusterCreateTime = Lude.Nothing,
      atRestEncryptionEnabled = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      securityGroups = Lude.Nothing,
      notificationConfiguration = Lude.Nothing,
      arn = Lude.Nothing,
      transitEncryptionEnabled = Lude.Nothing,
      snapshotWindow = Lude.Nothing,
      cacheClusterId = Lude.Nothing,
      configurationEndpoint = Lude.Nothing,
      engine = Lude.Nothing,
      cacheSecurityGroups = Lude.Nothing,
      authTokenEnabled = Lude.Nothing,
      clientDownloadLandingPage = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      cacheSubnetGroupName = Lude.Nothing,
      preferredAvailabilityZone = Lude.Nothing,
      cacheParameterGroup = Lude.Nothing,
      cacheClusterStatus = Lude.Nothing,
      snapshotRetentionLimit = Lude.Nothing,
      preferredOutpostARN = Lude.Nothing,
      replicationGroupId = Lude.Nothing,
      pendingModifiedValues = Lude.Nothing,
      numCacheNodes = Lude.Nothing
    }

-- | The date the auth token was last modified
--
-- /Note:/ Consider using 'authTokenLastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthTokenLastModifiedDate :: Lens.Lens' CacheCluster (Lude.Maybe Lude.DateTime)
ccAuthTokenLastModifiedDate = Lens.lens (authTokenLastModifiedDate :: CacheCluster -> Lude.Maybe Lude.DateTime) (\s a -> s {authTokenLastModifiedDate = a} :: CacheCluster)
{-# DEPRECATED ccAuthTokenLastModifiedDate "Use generic-lens or generic-optics with 'authTokenLastModifiedDate' instead." #-}

-- | The version of the cache engine that is used in this cluster.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngineVersion :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccEngineVersion = Lens.lens (engineVersion :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: CacheCluster)
{-# DEPRECATED ccEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

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
ccCacheNodeType :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccCacheNodeType = Lens.lens (cacheNodeType :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: CacheCluster)
{-# DEPRECATED ccCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | A list of cache nodes that are members of the cluster.
--
-- /Note:/ Consider using 'cacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheNodes :: Lens.Lens' CacheCluster (Lude.Maybe [CacheNode])
ccCacheNodes = Lens.lens (cacheNodes :: CacheCluster -> Lude.Maybe [CacheNode]) (\s a -> s {cacheNodes = a} :: CacheCluster)
{-# DEPRECATED ccCacheNodes "Use generic-lens or generic-optics with 'cacheNodes' instead." #-}

-- | The date and time when the cluster was created.
--
-- /Note:/ Consider using 'cacheClusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheClusterCreateTime :: Lens.Lens' CacheCluster (Lude.Maybe Lude.DateTime)
ccCacheClusterCreateTime = Lens.lens (cacheClusterCreateTime :: CacheCluster -> Lude.Maybe Lude.DateTime) (\s a -> s {cacheClusterCreateTime = a} :: CacheCluster)
{-# DEPRECATED ccCacheClusterCreateTime "Use generic-lens or generic-optics with 'cacheClusterCreateTime' instead." #-}

-- | A flag that enables encryption at-rest when set to @true@ .
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAtRestEncryptionEnabled :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Bool)
ccAtRestEncryptionEnabled = Lens.lens (atRestEncryptionEnabled :: CacheCluster -> Lude.Maybe Lude.Bool) (\s a -> s {atRestEncryptionEnabled = a} :: CacheCluster)
{-# DEPRECATED ccAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAutoMinorVersionUpgrade :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Bool)
ccAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: CacheCluster -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: CacheCluster)
{-# DEPRECATED ccAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | A list of VPC Security Groups associated with the cluster.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSecurityGroups :: Lens.Lens' CacheCluster (Lude.Maybe [SecurityGroupMembership])
ccSecurityGroups = Lens.lens (securityGroups :: CacheCluster -> Lude.Maybe [SecurityGroupMembership]) (\s a -> s {securityGroups = a} :: CacheCluster)
{-# DEPRECATED ccSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
--
-- /Note:/ Consider using 'notificationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNotificationConfiguration :: Lens.Lens' CacheCluster (Lude.Maybe NotificationConfiguration)
ccNotificationConfiguration = Lens.lens (notificationConfiguration :: CacheCluster -> Lude.Maybe NotificationConfiguration) (\s a -> s {notificationConfiguration = a} :: CacheCluster)
{-# DEPRECATED ccNotificationConfiguration "Use generic-lens or generic-optics with 'notificationConfiguration' instead." #-}

-- | The ARN (Amazon Resource Name) of the cache cluster.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccARN :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccARN = Lens.lens (arn :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CacheCluster)
{-# DEPRECATED ccARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A flag that enables in-transit encryption when set to @true@ .
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
-- __Required:__ Only available when creating a replication group in an Amazon VPC using redis version @3.2.6@ , @4.x@ or later.
-- Default: @false@
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccTransitEncryptionEnabled :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Bool)
ccTransitEncryptionEnabled = Lens.lens (transitEncryptionEnabled :: CacheCluster -> Lude.Maybe Lude.Bool) (\s a -> s {transitEncryptionEnabled = a} :: CacheCluster)
{-# DEPRECATED ccTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster.
--
-- Example: @05:00-09:00@
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnapshotWindow :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccSnapshotWindow = Lens.lens (snapshotWindow :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {snapshotWindow = a} :: CacheCluster)
{-# DEPRECATED ccSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheClusterId :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccCacheClusterId = Lens.lens (cacheClusterId :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: CacheCluster)
{-# DEPRECATED ccCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it.
--
-- Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@
--
-- /Note:/ Consider using 'configurationEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccConfigurationEndpoint :: Lens.Lens' CacheCluster (Lude.Maybe Endpoint)
ccConfigurationEndpoint = Lens.lens (configurationEndpoint :: CacheCluster -> Lude.Maybe Endpoint) (\s a -> s {configurationEndpoint = a} :: CacheCluster)
{-# DEPRECATED ccConfigurationEndpoint "Use generic-lens or generic-optics with 'configurationEndpoint' instead." #-}

-- | The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccEngine :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccEngine = Lens.lens (engine :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: CacheCluster)
{-# DEPRECATED ccEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A list of cache security group elements, composed of name and status sub-elements.
--
-- /Note:/ Consider using 'cacheSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheSecurityGroups :: Lens.Lens' CacheCluster (Lude.Maybe [CacheSecurityGroupMembership])
ccCacheSecurityGroups = Lens.lens (cacheSecurityGroups :: CacheCluster -> Lude.Maybe [CacheSecurityGroupMembership]) (\s a -> s {cacheSecurityGroups = a} :: CacheCluster)
{-# DEPRECATED ccCacheSecurityGroups "Use generic-lens or generic-optics with 'cacheSecurityGroups' instead." #-}

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands.
--
-- Default: @false@
--
-- /Note:/ Consider using 'authTokenEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccAuthTokenEnabled :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Bool)
ccAuthTokenEnabled = Lens.lens (authTokenEnabled :: CacheCluster -> Lude.Maybe Lude.Bool) (\s a -> s {authTokenEnabled = a} :: CacheCluster)
{-# DEPRECATED ccAuthTokenEnabled "Use generic-lens or generic-optics with 'authTokenEnabled' instead." #-}

-- | The URL of the web page where you can download the latest ElastiCache client library.
--
-- /Note:/ Consider using 'clientDownloadLandingPage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccClientDownloadLandingPage :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccClientDownloadLandingPage = Lens.lens (clientDownloadLandingPage :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {clientDownloadLandingPage = a} :: CacheCluster)
{-# DEPRECATED ccClientDownloadLandingPage "Use generic-lens or generic-optics with 'clientDownloadLandingPage' instead." #-}

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
ccPreferredMaintenanceWindow :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: CacheCluster)
{-# DEPRECATED ccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | The name of the cache subnet group associated with the cluster.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheSubnetGroupName :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: CacheCluster)
{-# DEPRECATED ccCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
--
-- /Note:/ Consider using 'preferredAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredAvailabilityZone :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccPreferredAvailabilityZone = Lens.lens (preferredAvailabilityZone :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredAvailabilityZone = a} :: CacheCluster)
{-# DEPRECATED ccPreferredAvailabilityZone "Use generic-lens or generic-optics with 'preferredAvailabilityZone' instead." #-}

-- | Status of the cache parameter group.
--
-- /Note:/ Consider using 'cacheParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheParameterGroup :: Lens.Lens' CacheCluster (Lude.Maybe CacheParameterGroupStatus)
ccCacheParameterGroup = Lens.lens (cacheParameterGroup :: CacheCluster -> Lude.Maybe CacheParameterGroupStatus) (\s a -> s {cacheParameterGroup = a} :: CacheCluster)
{-# DEPRECATED ccCacheParameterGroup "Use generic-lens or generic-optics with 'cacheParameterGroup' instead." #-}

-- | The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
--
-- /Note:/ Consider using 'cacheClusterStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCacheClusterStatus :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccCacheClusterStatus = Lens.lens (cacheClusterStatus :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterStatus = a} :: CacheCluster)
{-# DEPRECATED ccCacheClusterStatus "Use generic-lens or generic-optics with 'cacheClusterStatus' instead." #-}

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccSnapshotRetentionLimit :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Int)
ccSnapshotRetentionLimit = Lens.lens (snapshotRetentionLimit :: CacheCluster -> Lude.Maybe Lude.Int) (\s a -> s {snapshotRetentionLimit = a} :: CacheCluster)
{-# DEPRECATED ccSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The outpost ARN in which the cache cluster is created.
--
-- /Note:/ Consider using 'preferredOutpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPreferredOutpostARN :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccPreferredOutpostARN = Lens.lens (preferredOutpostARN :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredOutpostARN = a} :: CacheCluster)
{-# DEPRECATED ccPreferredOutpostARN "Use generic-lens or generic-optics with 'preferredOutpostARN' instead." #-}

-- | The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccReplicationGroupId :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Text)
ccReplicationGroupId = Lens.lens (replicationGroupId :: CacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupId = a} :: CacheCluster)
{-# DEPRECATED ccReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'pendingModifiedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccPendingModifiedValues :: Lens.Lens' CacheCluster (Lude.Maybe PendingModifiedValues)
ccPendingModifiedValues = Lens.lens (pendingModifiedValues :: CacheCluster -> Lude.Maybe PendingModifiedValues) (\s a -> s {pendingModifiedValues = a} :: CacheCluster)
{-# DEPRECATED ccPendingModifiedValues "Use generic-lens or generic-optics with 'pendingModifiedValues' instead." #-}

-- | The number of cache nodes in the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccNumCacheNodes :: Lens.Lens' CacheCluster (Lude.Maybe Lude.Int)
ccNumCacheNodes = Lens.lens (numCacheNodes :: CacheCluster -> Lude.Maybe Lude.Int) (\s a -> s {numCacheNodes = a} :: CacheCluster)
{-# DEPRECATED ccNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

instance Lude.FromXML CacheCluster where
  parseXML x =
    CacheCluster'
      Lude.<$> (x Lude..@? "AuthTokenLastModifiedDate")
      Lude.<*> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "CacheNodeType")
      Lude.<*> ( x Lude..@? "CacheNodes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheNode")
               )
      Lude.<*> (x Lude..@? "CacheClusterCreateTime")
      Lude.<*> (x Lude..@? "AtRestEncryptionEnabled")
      Lude.<*> (x Lude..@? "AutoMinorVersionUpgrade")
      Lude.<*> ( x Lude..@? "SecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "NotificationConfiguration")
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "TransitEncryptionEnabled")
      Lude.<*> (x Lude..@? "SnapshotWindow")
      Lude.<*> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "ConfigurationEndpoint")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> ( x Lude..@? "CacheSecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "CacheSecurityGroup")
               )
      Lude.<*> (x Lude..@? "AuthTokenEnabled")
      Lude.<*> (x Lude..@? "ClientDownloadLandingPage")
      Lude.<*> (x Lude..@? "PreferredMaintenanceWindow")
      Lude.<*> (x Lude..@? "CacheSubnetGroupName")
      Lude.<*> (x Lude..@? "PreferredAvailabilityZone")
      Lude.<*> (x Lude..@? "CacheParameterGroup")
      Lude.<*> (x Lude..@? "CacheClusterStatus")
      Lude.<*> (x Lude..@? "SnapshotRetentionLimit")
      Lude.<*> (x Lude..@? "PreferredOutpostArn")
      Lude.<*> (x Lude..@? "ReplicationGroupId")
      Lude.<*> (x Lude..@? "PendingModifiedValues")
      Lude.<*> (x Lude..@? "NumCacheNodes")
