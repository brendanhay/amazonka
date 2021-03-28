{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ModifyCacheCluster (..)
    , mkModifyCacheCluster
    -- ** Request lenses
    , mccCacheClusterId
    , mccAZMode
    , mccApplyImmediately
    , mccAuthToken
    , mccAuthTokenUpdateStrategy
    , mccAutoMinorVersionUpgrade
    , mccCacheNodeIdsToRemove
    , mccCacheNodeType
    , mccCacheParameterGroupName
    , mccCacheSecurityGroupNames
    , mccEngineVersion
    , mccNewAvailabilityZones
    , mccNotificationTopicArn
    , mccNotificationTopicStatus
    , mccNumCacheNodes
    , mccPreferredMaintenanceWindow
    , mccSecurityGroupIds
    , mccSnapshotRetentionLimit
    , mccSnapshotWindow

    -- * Destructuring the response
    , ModifyCacheClusterResponse (..)
    , mkModifyCacheClusterResponse
    -- ** Response lenses
    , mccrrsCacheCluster
    , mccrrsResponseStatus
    ) where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyCacheCluster@ operation.
--
-- /See:/ 'mkModifyCacheCluster' smart constructor.
data ModifyCacheCluster = ModifyCacheCluster'
  { cacheClusterId :: Core.Text
    -- ^ The cluster identifier. This value is stored as a lowercase string.
  , aZMode :: Core.Maybe Types.AZMode
    -- ^ Specifies whether the new nodes in this Memcached cluster are all created in a single Availability Zone or created across multiple Availability Zones.
--
-- Valid values: @single-az@ | @cross-az@ .
-- This option is only supported for Memcached clusters.
  , applyImmediately :: Core.Maybe Core.Bool
    -- ^ If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the cluster.
--
-- If @false@ , changes to the cluster are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
-- /Important:/ If you perform a @ModifyCacheCluster@ before a pending modification is applied, the pending modification is replaced by the newer modification.
-- Valid values: @true@ | @false@ 
-- Default: @false@ 
  , authToken :: Core.Maybe Core.Text
    -- ^ Reserved parameter. The password used to access a password protected server. This parameter must be specified with the @auth-token-update@ parameter. Password constraints:
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
  , authTokenUpdateStrategy :: Core.Maybe Types.AuthTokenUpdateStrategyType
    -- ^ Specifies the strategy to use to update the AUTH token. This parameter must be specified with the @auth-token@ parameter. Possible values:
--
--
--     * Rotate
--
--
--     * Set
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH> 
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ This parameter is currently disabled.
  , cacheNodeIdsToRemove :: Core.Maybe [Core.Text]
    -- ^ A list of cache node IDs to be removed. A node ID is a numeric identifier (0001, 0002, etc.). This parameter is only valid when @NumCacheNodes@ is less than the existing number of cache nodes. The number of cache node IDs supplied in this parameter must match the difference between the existing number of cache nodes in the cluster or pending cache nodes, whichever is greater, and the value of @NumCacheNodes@ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes, and the number of cache nodes in this @ModifyCacheCluster@ call is 5, you must list 2 (7 - 5) cache node IDs to remove.
  , cacheNodeType :: Core.Maybe Core.Text
    -- ^ A valid cache node type that you want to scale this cluster up to.
  , cacheParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cache parameter group to apply to this cluster. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
  , cacheSecurityGroupNames :: Core.Maybe [Core.Text]
    -- ^ A list of cache security group names to authorize on this cluster. This change is asynchronously applied as soon as possible.
--
-- You can use this parameter only with clusters that are created outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be "Default".
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The upgraded version of the cache engine to be run on the cache nodes.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster and create it anew with the earlier engine version. 
  , newAvailabilityZones :: Core.Maybe [Core.Text]
    -- ^ The list of Availability Zones where the new Memcached cache nodes are created.
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
  , notificationTopicArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
  , notificationTopicStatus :: Core.Maybe Core.Text
    -- ^ The status of the Amazon SNS notification topic. Notifications are sent only if the status is @active@ .
--
-- Valid values: @active@ | @inactive@ 
  , numCacheNodes :: Core.Maybe Core.Int
    -- ^ The number of cache nodes that the cluster should have. If the value for @NumCacheNodes@ is greater than the sum of the number of current cache nodes and the number of cache nodes pending creation (which may be zero), more nodes are added. If the value is less than the number of existing cache nodes, nodes are removed. If the value is equal to the number of current cache nodes, any pending add or remove requests are canceled.
--
-- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@ parameter to provide the IDs of the specific cache nodes to remove.
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
  , preferredMaintenanceWindow :: Core.Maybe Core.Text
    -- ^ Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period.
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
  , securityGroupIds :: Core.Maybe [Core.Text]
    -- ^ Specifies the VPC Security Groups associated with the cluster.
--
-- This parameter can be used only with clusters that are created in an Amazon Virtual Private Cloud (Amazon VPC).
  , snapshotRetentionLimit :: Core.Maybe Core.Int
    -- ^ The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
  , snapshotWindow :: Core.Maybe Core.Text
    -- ^ The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyCacheCluster' value with any optional fields omitted.
mkModifyCacheCluster
    :: Core.Text -- ^ 'cacheClusterId'
    -> ModifyCacheCluster
mkModifyCacheCluster cacheClusterId
  = ModifyCacheCluster'{cacheClusterId, aZMode = Core.Nothing,
                        applyImmediately = Core.Nothing, authToken = Core.Nothing,
                        authTokenUpdateStrategy = Core.Nothing,
                        autoMinorVersionUpgrade = Core.Nothing,
                        cacheNodeIdsToRemove = Core.Nothing, cacheNodeType = Core.Nothing,
                        cacheParameterGroupName = Core.Nothing,
                        cacheSecurityGroupNames = Core.Nothing,
                        engineVersion = Core.Nothing, newAvailabilityZones = Core.Nothing,
                        notificationTopicArn = Core.Nothing,
                        notificationTopicStatus = Core.Nothing,
                        numCacheNodes = Core.Nothing,
                        preferredMaintenanceWindow = Core.Nothing,
                        securityGroupIds = Core.Nothing,
                        snapshotRetentionLimit = Core.Nothing,
                        snapshotWindow = Core.Nothing}

-- | The cluster identifier. This value is stored as a lowercase string.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheClusterId :: Lens.Lens' ModifyCacheCluster Core.Text
mccCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE mccCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | Specifies whether the new nodes in this Memcached cluster are all created in a single Availability Zone or created across multiple Availability Zones.
--
-- Valid values: @single-az@ | @cross-az@ .
-- This option is only supported for Memcached clusters.
--
-- /Note:/ Consider using 'aZMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccAZMode :: Lens.Lens' ModifyCacheCluster (Core.Maybe Types.AZMode)
mccAZMode = Lens.field @"aZMode"
{-# INLINEABLE mccAZMode #-}
{-# DEPRECATED aZMode "Use generic-lens or generic-optics with 'aZMode' instead"  #-}

-- | If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the cluster.
--
-- If @false@ , changes to the cluster are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
-- /Important:/ If you perform a @ModifyCacheCluster@ before a pending modification is applied, the pending modification is replaced by the newer modification.
-- Valid values: @true@ | @false@ 
-- Default: @false@ 
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccApplyImmediately :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Bool)
mccApplyImmediately = Lens.field @"applyImmediately"
{-# INLINEABLE mccApplyImmediately #-}
{-# DEPRECATED applyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead"  #-}

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
mccAuthToken :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccAuthToken = Lens.field @"authToken"
{-# INLINEABLE mccAuthToken #-}
{-# DEPRECATED authToken "Use generic-lens or generic-optics with 'authToken' instead"  #-}

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
mccAuthTokenUpdateStrategy :: Lens.Lens' ModifyCacheCluster (Core.Maybe Types.AuthTokenUpdateStrategyType)
mccAuthTokenUpdateStrategy = Lens.field @"authTokenUpdateStrategy"
{-# INLINEABLE mccAuthTokenUpdateStrategy #-}
{-# DEPRECATED authTokenUpdateStrategy "Use generic-lens or generic-optics with 'authTokenUpdateStrategy' instead"  #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccAutoMinorVersionUpgrade :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Bool)
mccAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE mccAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | A list of cache node IDs to be removed. A node ID is a numeric identifier (0001, 0002, etc.). This parameter is only valid when @NumCacheNodes@ is less than the existing number of cache nodes. The number of cache node IDs supplied in this parameter must match the difference between the existing number of cache nodes in the cluster or pending cache nodes, whichever is greater, and the value of @NumCacheNodes@ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes, and the number of cache nodes in this @ModifyCacheCluster@ call is 5, you must list 2 (7 - 5) cache node IDs to remove.
--
-- /Note:/ Consider using 'cacheNodeIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheNodeIdsToRemove :: Lens.Lens' ModifyCacheCluster (Core.Maybe [Core.Text])
mccCacheNodeIdsToRemove = Lens.field @"cacheNodeIdsToRemove"
{-# INLINEABLE mccCacheNodeIdsToRemove #-}
{-# DEPRECATED cacheNodeIdsToRemove "Use generic-lens or generic-optics with 'cacheNodeIdsToRemove' instead"  #-}

-- | A valid cache node type that you want to scale this cluster up to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheNodeType :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccCacheNodeType = Lens.field @"cacheNodeType"
{-# INLINEABLE mccCacheNodeType #-}
{-# DEPRECATED cacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead"  #-}

-- | The name of the cache parameter group to apply to this cluster. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheParameterGroupName :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# INLINEABLE mccCacheParameterGroupName #-}
{-# DEPRECATED cacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead"  #-}

-- | A list of cache security group names to authorize on this cluster. This change is asynchronously applied as soon as possible.
--
-- You can use this parameter only with clusters that are created outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be "Default".
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccCacheSecurityGroupNames :: Lens.Lens' ModifyCacheCluster (Core.Maybe [Core.Text])
mccCacheSecurityGroupNames = Lens.field @"cacheSecurityGroupNames"
{-# INLINEABLE mccCacheSecurityGroupNames #-}
{-# DEPRECATED cacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead"  #-}

-- | The upgraded version of the cache engine to be run on the cache nodes.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cluster and create it anew with the earlier engine version. 
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccEngineVersion :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE mccEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

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
mccNewAvailabilityZones :: Lens.Lens' ModifyCacheCluster (Core.Maybe [Core.Text])
mccNewAvailabilityZones = Lens.field @"newAvailabilityZones"
{-# INLINEABLE mccNewAvailabilityZones #-}
{-# DEPRECATED newAvailabilityZones "Use generic-lens or generic-optics with 'newAvailabilityZones' instead"  #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccNotificationTopicArn :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccNotificationTopicArn = Lens.field @"notificationTopicArn"
{-# INLINEABLE mccNotificationTopicArn #-}
{-# DEPRECATED notificationTopicArn "Use generic-lens or generic-optics with 'notificationTopicArn' instead"  #-}

-- | The status of the Amazon SNS notification topic. Notifications are sent only if the status is @active@ .
--
-- Valid values: @active@ | @inactive@ 
--
-- /Note:/ Consider using 'notificationTopicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccNotificationTopicStatus :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccNotificationTopicStatus = Lens.field @"notificationTopicStatus"
{-# INLINEABLE mccNotificationTopicStatus #-}
{-# DEPRECATED notificationTopicStatus "Use generic-lens or generic-optics with 'notificationTopicStatus' instead"  #-}

-- | The number of cache nodes that the cluster should have. If the value for @NumCacheNodes@ is greater than the sum of the number of current cache nodes and the number of cache nodes pending creation (which may be zero), more nodes are added. If the value is less than the number of existing cache nodes, nodes are removed. If the value is equal to the number of current cache nodes, any pending add or remove requests are canceled.
--
-- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@ parameter to provide the IDs of the specific cache nodes to remove.
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- /Note:/ Consider using 'numCacheNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccNumCacheNodes :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Int)
mccNumCacheNodes = Lens.field @"numCacheNodes"
{-# INLINEABLE mccNumCacheNodes #-}
{-# DEPRECATED numCacheNodes "Use generic-lens or generic-optics with 'numCacheNodes' instead"  #-}

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
mccPreferredMaintenanceWindow :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# INLINEABLE mccPreferredMaintenanceWindow #-}
{-# DEPRECATED preferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead"  #-}

-- | Specifies the VPC Security Groups associated with the cluster.
--
-- This parameter can be used only with clusters that are created in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccSecurityGroupIds :: Lens.Lens' ModifyCacheCluster (Core.Maybe [Core.Text])
mccSecurityGroupIds = Lens.field @"securityGroupIds"
{-# INLINEABLE mccSecurityGroupIds #-}
{-# DEPRECATED securityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead"  #-}

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccSnapshotRetentionLimit :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Int)
mccSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# INLINEABLE mccSnapshotRetentionLimit #-}
{-# DEPRECATED snapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead"  #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster. 
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccSnapshotWindow :: Lens.Lens' ModifyCacheCluster (Core.Maybe Core.Text)
mccSnapshotWindow = Lens.field @"snapshotWindow"
{-# INLINEABLE mccSnapshotWindow #-}
{-# DEPRECATED snapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead"  #-}

instance Core.ToQuery ModifyCacheCluster where
        toQuery ModifyCacheCluster{..}
          = Core.toQueryPair "Action" ("ModifyCacheCluster" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-02-02" :: Core.Text)
              Core.<> Core.toQueryPair "CacheClusterId" cacheClusterId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "AZMode") aZMode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ApplyImmediately")
                applyImmediately
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AuthToken") authToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AuthTokenUpdateStrategy")
                authTokenUpdateStrategy
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoMinorVersionUpgrade")
                autoMinorVersionUpgrade
              Core.<>
              Core.toQueryPair "CacheNodeIdsToRemove"
                (Core.maybe Core.mempty (Core.toQueryList "CacheNodeId")
                   cacheNodeIdsToRemove)
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
              Core.maybe Core.mempty (Core.toQueryPair "EngineVersion")
                engineVersion
              Core.<>
              Core.toQueryPair "NewAvailabilityZones"
                (Core.maybe Core.mempty
                   (Core.toQueryList "PreferredAvailabilityZone")
                   newAvailabilityZones)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NotificationTopicArn")
                notificationTopicArn
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NotificationTopicStatus")
                notificationTopicStatus
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NumCacheNodes")
                numCacheNodes
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PreferredMaintenanceWindow")
                preferredMaintenanceWindow
              Core.<>
              Core.toQueryPair "SecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId")
                   securityGroupIds)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotRetentionLimit")
                snapshotRetentionLimit
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SnapshotWindow")
                snapshotWindow

instance Core.ToHeaders ModifyCacheCluster where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyCacheCluster where
        type Rs ModifyCacheCluster = ModifyCacheClusterResponse
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
          = Response.receiveXMLWrapper "ModifyCacheClusterResult"
              (\ s h x ->
                 ModifyCacheClusterResponse' Core.<$>
                   (x Core..@? "CacheCluster") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyCacheClusterResponse' smart constructor.
data ModifyCacheClusterResponse = ModifyCacheClusterResponse'
  { cacheCluster :: Core.Maybe Types.CacheCluster
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ModifyCacheClusterResponse' value with any optional fields omitted.
mkModifyCacheClusterResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyCacheClusterResponse
mkModifyCacheClusterResponse responseStatus
  = ModifyCacheClusterResponse'{cacheCluster = Core.Nothing,
                                responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'cacheCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccrrsCacheCluster :: Lens.Lens' ModifyCacheClusterResponse (Core.Maybe Types.CacheCluster)
mccrrsCacheCluster = Lens.field @"cacheCluster"
{-# INLINEABLE mccrrsCacheCluster #-}
{-# DEPRECATED cacheCluster "Use generic-lens or generic-optics with 'cacheCluster' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccrrsResponseStatus :: Lens.Lens' ModifyCacheClusterResponse Core.Int
mccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
