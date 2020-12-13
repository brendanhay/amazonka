{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyCacheCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a cluster. You can use this operation to change one or more cluster configuration parameters by specifying the parameters and the new values.
module Network.AWS.ElastiCache.ModifyCacheCluster
  ( -- * Creating a request
    ModifyCacheCluster (..),
    mkModifyCacheCluster,

    -- ** Request lenses
    mccEngineVersion,
    mccCacheNodeType,
    mccSecurityGroupIds,
    mccAutoMinorVersionUpgrade,
    mccCacheParameterGroupName,
    mccSnapshotWindow,
    mccNewAvailabilityZones,
    mccCacheClusterId,
    mccAuthToken,
    mccPreferredMaintenanceWindow,
    mccCacheNodeIdsToRemove,
    mccSnapshotRetentionLimit,
    mccNotificationTopicStatus,
    mccAZMode,
    mccApplyImmediately,
    mccAuthTokenUpdateStrategy,
    mccNotificationTopicARN,
    mccNumCacheNodes,
    mccCacheSecurityGroupNames,

    -- * Destructuring the response
    ModifyCacheClusterResponse (..),
    mkModifyCacheClusterResponse,

    -- ** Response lenses
    mccrsCacheCluster,
    mccrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ModifyCacheCluster@ operation.
--
-- /See:/ 'mkModifyCacheCluster' smart constructor.
data ModifyCacheCluster = ModifyCacheCluster'
  { -- | The upgraded version of the cache engine to be run on the cache nodes.
    --
    -- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster and create it anew with the earlier engine version.
    engineVersion :: Lude.Maybe Lude.Text,
    -- | A valid cache node type that you want to scale this cluster up to.
    cacheNodeType :: Lude.Maybe Lude.Text,
    -- | Specifies the VPC Security Groups associated with the cluster.
    --
    -- This parameter can be used only with clusters that are created in an Amazon Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
    -- | The name of the cache parameter group to apply to this cluster. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
    cacheParameterGroupName :: Lude.Maybe Lude.Text,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster.
    snapshotWindow :: Lude.Maybe Lude.Text,
    -- | The list of Availability Zones where the new Memcached cache nodes are created.
    --
    -- This parameter is only valid when @NumCacheNodes@ in the request is greater than the sum of the number of active cache nodes and the number of cache nodes pending creation (which may be zero). The number of Availability Zones supplied in this list must match the cache nodes being added in this request.
    -- This option is only supported on Memcached clusters.
    -- Scenarios:
    --
    --     * __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes. Specify @NumCacheNodes=5@ (3 + 2) and optionally specify two Availability Zones for the two new nodes.
    --
    --
    --     * __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation (from the scenario 1 call) and want to add 1 more node. Specify @NumCacheNodes=6@ ((3 + 2) + 1) and optionally specify an Availability Zone for the new node.
    --
    --
    --     * __Scenario 3:__ You want to cancel all pending operations. Specify @NumCacheNodes=3@ to cancel all pending operations.
    --
    --
    -- The Availability Zone placement of nodes pending creation cannot be modified. If you wish to cancel any nodes pending creation, add 0 nodes by setting @NumCacheNodes@ to the number of current nodes.
    -- If @cross-az@ is specified, existing Memcached nodes remain in their current Availability Zone. Only newly created nodes can be located in different Availability Zones. For guidance on how to move existing Memcached nodes to different Availability Zones, see the __Availability Zone Considerations__ section of <https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/CacheNodes.SupportedTypes.html Cache Node Considerations for Memcached> .
    -- __Impact of new add/remove requests upon pending requests__
    --
    --     * Scenario-1
    --
    --     * Pending Action: Delete
    --
    --
    --     * New Request: Delete
    --
    --
    --     * Result: The new delete, pending or immediate, replaces the pending delete.
    --
    --
    --
    --
    --     * Scenario-2
    --
    --     * Pending Action: Delete
    --
    --
    --     * New Request: Create
    --
    --
    --     * Result: The new create, pending or immediate, replaces the pending delete.
    --
    --
    --
    --
    --     * Scenario-3
    --
    --     * Pending Action: Create
    --
    --
    --     * New Request: Delete
    --
    --
    --     * Result: The new delete, pending or immediate, replaces the pending create.
    --
    --
    --
    --
    --     * Scenario-4
    --
    --     * Pending Action: Create
    --
    --
    --     * New Request: Create
    --
    --
    --     * Result: The new create is added to the pending create.
    -- /Important:/ __Important:__ If the new create request is __Apply Immediately - Yes__ , all creates are performed immediately. If the new create request is __Apply Immediately - No__ , all creates are pending.
    newAvailabilityZones :: Lude.Maybe [Lude.Text],
    -- | The cluster identifier. This value is stored as a lowercase string.
    cacheClusterId :: Lude.Text,
    -- | Reserved parameter. The password used to access a password protected server. This parameter must be specified with the @auth-token-update@ parameter. Password constraints:
    --
    --
    --     * Must be only printable ASCII characters
    --
    --
    --     * Must be at least 16 characters and no more than 128 characters in length
    --
    --
    --     * Cannot contain any of the following characters: '/', '"', or '@', '%'
    --
    --
    -- For more information, see AUTH password at <http://redis.io/commands/AUTH AUTH> .
    authToken :: Lude.Maybe Lude.Text,
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
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    -- | A list of cache node IDs to be removed. A node ID is a numeric identifier (0001, 0002, etc.). This parameter is only valid when @NumCacheNodes@ is less than the existing number of cache nodes. The number of cache node IDs supplied in this parameter must match the difference between the existing number of cache nodes in the cluster or pending cache nodes, whichever is greater, and the value of @NumCacheNodes@ in the request.
    --
    -- For example: If you have 3 active cache nodes, 7 pending cache nodes, and the number of cache nodes in this @ModifyCacheCluster@ call is 5, you must list 2 (7 - 5) cache node IDs to remove.
    cacheNodeIdsToRemove :: Lude.Maybe [Lude.Text],
    -- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
    snapshotRetentionLimit :: Lude.Maybe Lude.Int,
    -- | The status of the Amazon SNS notification topic. Notifications are sent only if the status is @active@ .
    --
    -- Valid values: @active@ | @inactive@
    notificationTopicStatus :: Lude.Maybe Lude.Text,
    -- | Specifies whether the new nodes in this Memcached cluster are all created in a single Availability Zone or created across multiple Availability Zones.
    --
    -- Valid values: @single-az@ | @cross-az@ .
    -- This option is only supported for Memcached clusters.
    aZMode :: Lude.Maybe AZMode,
    -- | If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the cluster.
    --
    -- If @false@ , changes to the cluster are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
    -- /Important:/ If you perform a @ModifyCacheCluster@ before a pending modification is applied, the pending modification is replaced by the newer modification.
    -- Valid values: @true@ | @false@
    -- Default: @false@
    applyImmediately :: Lude.Maybe Lude.Bool,
    -- | Specifies the strategy to use to update the AUTH token. This parameter must be specified with the @auth-token@ parameter. Possible values:
    --
    --
    --     * Rotate
    --
    --
    --     * Set
    --
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
    authTokenUpdateStrategy :: Lude.Maybe AuthTokenUpdateStrategyType,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
    notificationTopicARN :: Lude.Maybe Lude.Text,
    -- | The number of cache nodes that the cluster should have. If the value for @NumCacheNodes@ is greater than the sum of the number of current cache nodes and the number of cache nodes pending creation (which may be zero), more nodes are added. If the value is less than the number of existing cache nodes, nodes are removed. If the value is equal to the number of current cache nodes, any pending add or remove requests are canceled.
    --
    -- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@ parameter to provide the IDs of the specific cache nodes to remove.
    -- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
    numCacheNodes :: Lude.Maybe Lude.Int,
    -- | A list of cache security group names to authorize on this cluster. This change is asynchronously applied as soon as possible.
    --
    -- You can use this parameter only with clusters that are created outside of an Amazon Virtual Private Cloud (Amazon VPC).
    -- Constraints: Must contain no more than 255 alphanumeric characters. Must not be "Default".
    cacheSecurityGroupNames :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCacheCluster' with the minimum fields required to make a request.
--
-- * 'engineVersion' - The upgraded version of the cache engine to be run on the cache nodes.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster and create it anew with the earlier engine version.
-- * 'cacheNodeType' - A valid cache node type that you want to scale this cluster up to.
-- * 'securityGroupIds' - Specifies the VPC Security Groups associated with the cluster.
--
-- This parameter can be used only with clusters that are created in an Amazon Virtual Private Cloud (Amazon VPC).
-- * 'autoMinorVersionUpgrade' - This parameter is currently disabled.
-- * 'cacheParameterGroupName' - The name of the cache parameter group to apply to this cluster. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
-- * 'snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster.
-- * 'newAvailabilityZones' - The list of Availability Zones where the new Memcached cache nodes are created.
--
-- This parameter is only valid when @NumCacheNodes@ in the request is greater than the sum of the number of active cache nodes and the number of cache nodes pending creation (which may be zero). The number of Availability Zones supplied in this list must match the cache nodes being added in this request.
-- This option is only supported on Memcached clusters.
-- Scenarios:
--
--     * __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes. Specify @NumCacheNodes=5@ (3 + 2) and optionally specify two Availability Zones for the two new nodes.
--
--
--     * __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation (from the scenario 1 call) and want to add 1 more node. Specify @NumCacheNodes=6@ ((3 + 2) + 1) and optionally specify an Availability Zone for the new node.
--
--
--     * __Scenario 3:__ You want to cancel all pending operations. Specify @NumCacheNodes=3@ to cancel all pending operations.
--
--
-- The Availability Zone placement of nodes pending creation cannot be modified. If you wish to cancel any nodes pending creation, add 0 nodes by setting @NumCacheNodes@ to the number of current nodes.
-- If @cross-az@ is specified, existing Memcached nodes remain in their current Availability Zone. Only newly created nodes can be located in different Availability Zones. For guidance on how to move existing Memcached nodes to different Availability Zones, see the __Availability Zone Considerations__ section of <https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/CacheNodes.SupportedTypes.html Cache Node Considerations for Memcached> .
-- __Impact of new add/remove requests upon pending requests__
--
--     * Scenario-1
--
--     * Pending Action: Delete
--
--
--     * New Request: Delete
--
--
--     * Result: The new delete, pending or immediate, replaces the pending delete.
--
--
--
--
--     * Scenario-2
--
--     * Pending Action: Delete
--
--
--     * New Request: Create
--
--
--     * Result: The new create, pending or immediate, replaces the pending delete.
--
--
--
--
--     * Scenario-3
--
--     * Pending Action: Create
--
--
--     * New Request: Delete
--
--
--     * Result: The new delete, pending or immediate, replaces the pending create.
--
--
--
--
--     * Scenario-4
--
--     * Pending Action: Create
--
--
--     * New Request: Create
--
--
--     * Result: The new create is added to the pending create.
-- /Important:/ __Important:__ If the new create request is __Apply Immediately - Yes__ , all creates are performed immediately. If the new create request is __Apply Immediately - No__ , all creates are pending.
--
--
--
--
-- * 'cacheClusterId' - The cluster identifier. This value is stored as a lowercase string.
-- * 'authToken' - Reserved parameter. The password used to access a password protected server. This parameter must be specified with the @auth-token-update@ parameter. Password constraints:
--
--
--     * Must be only printable ASCII characters
--
--
--     * Must be at least 16 characters and no more than 128 characters in length
--
--
--     * Cannot contain any of the following characters: '/', '"', or '@', '%'
--
--
-- For more information, see AUTH password at <http://redis.io/commands/AUTH AUTH> .
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
-- * 'cacheNodeIdsToRemove' - A list of cache node IDs to be removed. A node ID is a numeric identifier (0001, 0002, etc.). This parameter is only valid when @NumCacheNodes@ is less than the existing number of cache nodes. The number of cache node IDs supplied in this parameter must match the difference between the existing number of cache nodes in the cluster or pending cache nodes, whichever is greater, and the value of @NumCacheNodes@ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes, and the number of cache nodes in this @ModifyCacheCluster@ call is 5, you must list 2 (7 - 5) cache node IDs to remove.
-- * 'snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
-- * 'notificationTopicStatus' - The status of the Amazon SNS notification topic. Notifications are sent only if the status is @active@ .
--
-- Valid values: @active@ | @inactive@
-- * 'aZMode' - Specifies whether the new nodes in this Memcached cluster are all created in a single Availability Zone or created across multiple Availability Zones.
--
-- Valid values: @single-az@ | @cross-az@ .
-- This option is only supported for Memcached clusters.
-- * 'applyImmediately' - If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the cluster.
--
-- If @false@ , changes to the cluster are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
-- /Important:/ If you perform a @ModifyCacheCluster@ before a pending modification is applied, the pending modification is replaced by the newer modification.
-- Valid values: @true@ | @false@
-- Default: @false@
-- * 'authTokenUpdateStrategy' - Specifies the strategy to use to update the AUTH token. This parameter must be specified with the @auth-token@ parameter. Possible values:
--
--
--     * Rotate
--
--
--     * Set
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
-- * 'notificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
-- * 'numCacheNodes' - The number of cache nodes that the cluster should have. If the value for @NumCacheNodes@ is greater than the sum of the number of current cache nodes and the number of cache nodes pending creation (which may be zero), more nodes are added. If the value is less than the number of existing cache nodes, nodes are removed. If the value is equal to the number of current cache nodes, any pending add or remove requests are canceled.
--
-- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@ parameter to provide the IDs of the specific cache nodes to remove.
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
-- * 'cacheSecurityGroupNames' - A list of cache security group names to authorize on this cluster. This change is asynchronously applied as soon as possible.
--
-- You can use this parameter only with clusters that are created outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be "Default".
mkModifyCacheCluster ::
  -- | 'cacheClusterId'
  Lude.Text ->
  ModifyCacheCluster
mkModifyCacheCluster pCacheClusterId_ =
  ModifyCacheCluster'
    { engineVersion = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      cacheParameterGroupName = Lude.Nothing,
      snapshotWindow = Lude.Nothing,
      newAvailabilityZones = Lude.Nothing,
      cacheClusterId = pCacheClusterId_,
      authToken = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      cacheNodeIdsToRemove = Lude.Nothing,
      snapshotRetentionLimit = Lude.Nothing,
      notificationTopicStatus = Lude.Nothing,
      aZMode = Lude.Nothing,
      applyImmediately = Lude.Nothing,
      authTokenUpdateStrategy = Lude.Nothing,
      notificationTopicARN = Lude.Nothing,
      numCacheNodes = Lude.Nothing,
      cacheSecurityGroupNames = Lude.Nothing
    }

-- | The upgraded version of the cache engine to be run on the cache nodes.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster and create it anew with the earlier engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccEngineVersion :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccEngineVersion = Lens.lens (engineVersion :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ModifyCacheCluster)
{-# DEPRECATED mccEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A valid cache node type that you want to scale this cluster up to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheNodeType :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccCacheNodeType = Lens.lens (cacheNodeType :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: ModifyCacheCluster)
{-# DEPRECATED mccCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | Specifies the VPC Security Groups associated with the cluster.
--
-- This parameter can be used only with clusters that are created in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccSecurityGroupIds :: Lens.Lens' ModifyCacheCluster (Lude.Maybe [Lude.Text])
mccSecurityGroupIds = Lens.lens (securityGroupIds :: ModifyCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ModifyCacheCluster)
{-# DEPRECATED mccSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccAutoMinorVersionUpgrade :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Bool)
mccAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: ModifyCacheCluster -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: ModifyCacheCluster)
{-# DEPRECATED mccAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The name of the cache parameter group to apply to this cluster. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheParameterGroupName :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: ModifyCacheCluster)
{-# DEPRECATED mccCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccSnapshotWindow :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccSnapshotWindow = Lens.lens (snapshotWindow :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {snapshotWindow = a} :: ModifyCacheCluster)
{-# DEPRECATED mccSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | The list of Availability Zones where the new Memcached cache nodes are created.
--
-- This parameter is only valid when @NumCacheNodes@ in the request is greater than the sum of the number of active cache nodes and the number of cache nodes pending creation (which may be zero). The number of Availability Zones supplied in this list must match the cache nodes being added in this request.
-- This option is only supported on Memcached clusters.
-- Scenarios:
--
--     * __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes. Specify @NumCacheNodes=5@ (3 + 2) and optionally specify two Availability Zones for the two new nodes.
--
--
--     * __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation (from the scenario 1 call) and want to add 1 more node. Specify @NumCacheNodes=6@ ((3 + 2) + 1) and optionally specify an Availability Zone for the new node.
--
--
--     * __Scenario 3:__ You want to cancel all pending operations. Specify @NumCacheNodes=3@ to cancel all pending operations.
--
--
-- The Availability Zone placement of nodes pending creation cannot be modified. If you wish to cancel any nodes pending creation, add 0 nodes by setting @NumCacheNodes@ to the number of current nodes.
-- If @cross-az@ is specified, existing Memcached nodes remain in their current Availability Zone. Only newly created nodes can be located in different Availability Zones. For guidance on how to move existing Memcached nodes to different Availability Zones, see the __Availability Zone Considerations__ section of <https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/CacheNodes.SupportedTypes.html Cache Node Considerations for Memcached> .
-- __Impact of new add/remove requests upon pending requests__
--
--     * Scenario-1
--
--     * Pending Action: Delete
--
--
--     * New Request: Delete
--
--
--     * Result: The new delete, pending or immediate, replaces the pending delete.
--
--
--
--
--     * Scenario-2
--
--     * Pending Action: Delete
--
--
--     * New Request: Create
--
--
--     * Result: The new create, pending or immediate, replaces the pending delete.
--
--
--
--
--     * Scenario-3
--
--     * Pending Action: Create
--
--
--     * New Request: Delete
--
--
--     * Result: The new delete, pending or immediate, replaces the pending create.
--
--
--
--
--     * Scenario-4
--
--     * Pending Action: Create
--
--
--     * New Request: Create
--
--
--     * Result: The new create is added to the pending create.
-- /Important:/ __Important:__ If the new create request is __Apply Immediately - Yes__ , all creates are performed immediately. If the new create request is __Apply Immediately - No__ , all creates are pending.
--
--
--
--
--
-- /Note:/ Consider using 'newAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccNewAvailabilityZones :: Lens.Lens' ModifyCacheCluster (Lude.Maybe [Lude.Text])
mccNewAvailabilityZones = Lens.lens (newAvailabilityZones :: ModifyCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {newAvailabilityZones = a} :: ModifyCacheCluster)
{-# DEPRECATED mccNewAvailabilityZones "Use generic-lens or generic-optics with 'newAvailabilityZones' instead." #-}

-- | The cluster identifier. This value is stored as a lowercase string.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheClusterId :: Lens.Lens' ModifyCacheCluster Lude.Text
mccCacheClusterId = Lens.lens (cacheClusterId :: ModifyCacheCluster -> Lude.Text) (\s a -> s {cacheClusterId = a} :: ModifyCacheCluster)
{-# DEPRECATED mccCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | Reserved parameter. The password used to access a password protected server. This parameter must be specified with the @auth-token-update@ parameter. Password constraints:
--
--
--     * Must be only printable ASCII characters
--
--
--     * Must be at least 16 characters and no more than 128 characters in length
--
--
--     * Cannot contain any of the following characters: '/', '"', or '@', '%'
--
--
-- For more information, see AUTH password at <http://redis.io/commands/AUTH AUTH> .
--
-- /Note:/ Consider using 'authToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccAuthToken :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccAuthToken = Lens.lens (authToken :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {authToken = a} :: ModifyCacheCluster)
{-# DEPRECATED mccAuthToken "Use generic-lens or generic-optics with 'authToken' instead." #-}

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
mccPreferredMaintenanceWindow :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: ModifyCacheCluster)
{-# DEPRECATED mccPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | A list of cache node IDs to be removed. A node ID is a numeric identifier (0001, 0002, etc.). This parameter is only valid when @NumCacheNodes@ is less than the existing number of cache nodes. The number of cache node IDs supplied in this parameter must match the difference between the existing number of cache nodes in the cluster or pending cache nodes, whichever is greater, and the value of @NumCacheNodes@ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes, and the number of cache nodes in this @ModifyCacheCluster@ call is 5, you must list 2 (7 - 5) cache node IDs to remove.
--
-- /Note:/ Consider using 'cacheNodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheNodeIdsToRemove :: Lens.Lens' ModifyCacheCluster (Lude.Maybe [Lude.Text])
mccCacheNodeIdsToRemove = Lens.lens (cacheNodeIdsToRemove :: ModifyCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheNodeIdsToRemove = a} :: ModifyCacheCluster)
{-# DEPRECATED mccCacheNodeIdsToRemove "Use generic-lens or generic-optics with 'cacheNodeIdsToRemove' instead." #-}

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccSnapshotRetentionLimit :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Int)
mccSnapshotRetentionLimit = Lens.lens (snapshotRetentionLimit :: ModifyCacheCluster -> Lude.Maybe Lude.Int) (\s a -> s {snapshotRetentionLimit = a} :: ModifyCacheCluster)
{-# DEPRECATED mccSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The status of the Amazon SNS notification topic. Notifications are sent only if the status is @active@ .
--
-- Valid values: @active@ | @inactive@
--
-- /Note:/ Consider using 'notificationTopicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccNotificationTopicStatus :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccNotificationTopicStatus = Lens.lens (notificationTopicStatus :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicStatus = a} :: ModifyCacheCluster)
{-# DEPRECATED mccNotificationTopicStatus "Use generic-lens or generic-optics with 'notificationTopicStatus' instead." #-}

-- | Specifies whether the new nodes in this Memcached cluster are all created in a single Availability Zone or created across multiple Availability Zones.
--
-- Valid values: @single-az@ | @cross-az@ .
-- This option is only supported for Memcached clusters.
--
-- /Note:/ Consider using 'aZMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccAZMode :: Lens.Lens' ModifyCacheCluster (Lude.Maybe AZMode)
mccAZMode = Lens.lens (aZMode :: ModifyCacheCluster -> Lude.Maybe AZMode) (\s a -> s {aZMode = a} :: ModifyCacheCluster)
{-# DEPRECATED mccAZMode "Use generic-lens or generic-optics with 'aZMode' instead." #-}

-- | If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the cluster.
--
-- If @false@ , changes to the cluster are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
-- /Important:/ If you perform a @ModifyCacheCluster@ before a pending modification is applied, the pending modification is replaced by the newer modification.
-- Valid values: @true@ | @false@
-- Default: @false@
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccApplyImmediately :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Bool)
mccApplyImmediately = Lens.lens (applyImmediately :: ModifyCacheCluster -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyCacheCluster)
{-# DEPRECATED mccApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Specifies the strategy to use to update the AUTH token. This parameter must be specified with the @auth-token@ parameter. Possible values:
--
--
--     * Rotate
--
--
--     * Set
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
--
-- /Note:/ Consider using 'authTokenUpdateStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccAuthTokenUpdateStrategy :: Lens.Lens' ModifyCacheCluster (Lude.Maybe AuthTokenUpdateStrategyType)
mccAuthTokenUpdateStrategy = Lens.lens (authTokenUpdateStrategy :: ModifyCacheCluster -> Lude.Maybe AuthTokenUpdateStrategyType) (\s a -> s {authTokenUpdateStrategy = a} :: ModifyCacheCluster)
{-# DEPRECATED mccAuthTokenUpdateStrategy "Use generic-lens or generic-optics with 'authTokenUpdateStrategy' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccNotificationTopicARN :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Text)
mccNotificationTopicARN = Lens.lens (notificationTopicARN :: ModifyCacheCluster -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicARN = a} :: ModifyCacheCluster)
{-# DEPRECATED mccNotificationTopicARN "Use generic-lens or generic-optics with 'notificationTopicARN' instead." #-}

-- | The number of cache nodes that the cluster should have. If the value for @NumCacheNodes@ is greater than the sum of the number of current cache nodes and the number of cache nodes pending creation (which may be zero), more nodes are added. If the value is less than the number of existing cache nodes, nodes are removed. If the value is equal to the number of current cache nodes, any pending add or remove requests are canceled.
--
-- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@ parameter to provide the IDs of the specific cache nodes to remove.
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccNumCacheNodes :: Lens.Lens' ModifyCacheCluster (Lude.Maybe Lude.Int)
mccNumCacheNodes = Lens.lens (numCacheNodes :: ModifyCacheCluster -> Lude.Maybe Lude.Int) (\s a -> s {numCacheNodes = a} :: ModifyCacheCluster)
{-# DEPRECATED mccNumCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead." #-}

-- | A list of cache security group names to authorize on this cluster. This change is asynchronously applied as soon as possible.
--
-- You can use this parameter only with clusters that are created outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be "Default".
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheSecurityGroupNames :: Lens.Lens' ModifyCacheCluster (Lude.Maybe [Lude.Text])
mccCacheSecurityGroupNames = Lens.lens (cacheSecurityGroupNames :: ModifyCacheCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheSecurityGroupNames = a} :: ModifyCacheCluster)
{-# DEPRECATED mccCacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead." #-}

instance Lude.AWSRequest ModifyCacheCluster where
  type Rs ModifyCacheCluster = ModifyCacheClusterResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyCacheClusterResult"
      ( \s h x ->
          ModifyCacheClusterResponse'
            Lude.<$> (x Lude..@? "CacheCluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyCacheCluster where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyCacheCluster where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyCacheCluster where
  toQuery ModifyCacheCluster' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyCacheCluster" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "EngineVersion" Lude.=: engineVersion,
        "CacheNodeType" Lude.=: cacheNodeType,
        "SecurityGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "SnapshotWindow" Lude.=: snapshotWindow,
        "NewAvailabilityZones"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "PreferredAvailabilityZone"
                Lude.<$> newAvailabilityZones
            ),
        "CacheClusterId" Lude.=: cacheClusterId,
        "AuthToken" Lude.=: authToken,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "CacheNodeIdsToRemove"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "CacheNodeId" Lude.<$> cacheNodeIdsToRemove),
        "SnapshotRetentionLimit" Lude.=: snapshotRetentionLimit,
        "NotificationTopicStatus" Lude.=: notificationTopicStatus,
        "AZMode" Lude.=: aZMode,
        "ApplyImmediately" Lude.=: applyImmediately,
        "AuthTokenUpdateStrategy" Lude.=: authTokenUpdateStrategy,
        "NotificationTopicArn" Lude.=: notificationTopicARN,
        "NumCacheNodes" Lude.=: numCacheNodes,
        "CacheSecurityGroupNames"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "CacheSecurityGroupName"
                Lude.<$> cacheSecurityGroupNames
            )
      ]

-- | /See:/ 'mkModifyCacheClusterResponse' smart constructor.
data ModifyCacheClusterResponse = ModifyCacheClusterResponse'
  { cacheCluster :: Lude.Maybe CacheCluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyCacheClusterResponse' with the minimum fields required to make a request.
--
-- * 'cacheCluster' -
-- * 'responseStatus' - The response status code.
mkModifyCacheClusterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyCacheClusterResponse
mkModifyCacheClusterResponse pResponseStatus_ =
  ModifyCacheClusterResponse'
    { cacheCluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccrsCacheCluster :: Lens.Lens' ModifyCacheClusterResponse (Lude.Maybe CacheCluster)
mccrsCacheCluster = Lens.lens (cacheCluster :: ModifyCacheClusterResponse -> Lude.Maybe CacheCluster) (\s a -> s {cacheCluster = a} :: ModifyCacheClusterResponse)
{-# DEPRECATED mccrsCacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccrsResponseStatus :: Lens.Lens' ModifyCacheClusterResponse Lude.Int
mccrsResponseStatus = Lens.lens (responseStatus :: ModifyCacheClusterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyCacheClusterResponse)
{-# DEPRECATED mccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
