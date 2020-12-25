{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a replication group.
--
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/scaling-redis-cluster-mode-enabled.html Scaling for Amazon ElastiCache for Redis (cluster mode enabled)> in the ElastiCache User Guide
--
--
--     * <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyReplicationGroupShardConfiguration.html ModifyReplicationGroupShardConfiguration> in the ElastiCache API Reference
module Network.AWS.ElastiCache.ModifyReplicationGroup
  ( -- * Creating a request
    ModifyReplicationGroup (..),
    mkModifyReplicationGroup,

    -- ** Request lenses
    mrgReplicationGroupId,
    mrgApplyImmediately,
    mrgAuthToken,
    mrgAuthTokenUpdateStrategy,
    mrgAutoMinorVersionUpgrade,
    mrgAutomaticFailoverEnabled,
    mrgCacheNodeType,
    mrgCacheParameterGroupName,
    mrgCacheSecurityGroupNames,
    mrgEngineVersion,
    mrgMultiAZEnabled,
    mrgNodeGroupId,
    mrgNotificationTopicArn,
    mrgNotificationTopicStatus,
    mrgPreferredMaintenanceWindow,
    mrgPrimaryClusterId,
    mrgRemoveUserGroups,
    mrgReplicationGroupDescription,
    mrgSecurityGroupIds,
    mrgSnapshotRetentionLimit,
    mrgSnapshotWindow,
    mrgSnapshottingClusterId,
    mrgUserGroupIdsToAdd,
    mrgUserGroupIdsToRemove,

    -- * Destructuring the response
    ModifyReplicationGroupResponse (..),
    mkModifyReplicationGroupResponse,

    -- ** Response lenses
    mrgrrsReplicationGroup,
    mrgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyReplicationGroups@ operation.
--
-- /See:/ 'mkModifyReplicationGroup' smart constructor.
data ModifyReplicationGroup = ModifyReplicationGroup'
  { -- | The identifier of the replication group to modify.
    replicationGroupId :: Types.String,
    -- | If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the replication group.
    --
    -- If @false@ , changes to the nodes in the replication group are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
    -- Valid values: @true@ | @false@
    -- Default: @false@
    applyImmediately :: Core.Maybe Core.Bool,
    -- | Reserved parameter. The password used to access a password protected server. This parameter must be specified with the @auth-token-update-strategy @ parameter. Password constraints:
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
    authToken :: Core.Maybe Types.String,
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
    authTokenUpdateStrategy :: Core.Maybe Types.AuthTokenUpdateStrategyType,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
    --
    -- Valid values: @true@ | @false@
    automaticFailoverEnabled :: Core.Maybe Core.Bool,
    -- | A valid cache node type that you want to scale this replication group to.
    cacheNodeType :: Core.Maybe Types.String,
    -- | The name of the cache parameter group to apply to all of the clusters in this replication group. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
    cacheParameterGroupName :: Core.Maybe Types.String,
    -- | A list of cache security group names to authorize for the clusters in this replication group. This change is asynchronously applied as soon as possible.
    --
    -- This parameter can be used only with replication group containing clusters running outside of an Amazon Virtual Private Cloud (Amazon VPC).
    -- Constraints: Must contain no more than 255 alphanumeric characters. Must not be @Default@ .
    cacheSecurityGroupNames :: Core.Maybe [Types.String],
    -- | The upgraded version of the cache engine to be run on the clusters in the replication group.
    --
    -- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing replication group and create it anew with the earlier engine version.
    engineVersion :: Core.Maybe Types.String,
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
    multiAZEnabled :: Core.Maybe Core.Bool,
    -- | Deprecated. This parameter is not used.
    nodeGroupId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
    notificationTopicArn :: Core.Maybe Types.String,
    -- | The status of the Amazon SNS notification topic for the replication group. Notifications are sent only if the status is @active@ .
    --
    -- Valid values: @active@ | @inactive@
    notificationTopicStatus :: Core.Maybe Types.String,
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
    -- | For replication groups with a single primary, if this parameter is specified, ElastiCache promotes the specified cluster in the specified replication group to the primary role. The nodes of all other clusters in the replication group are read replicas.
    primaryClusterId :: Core.Maybe Types.String,
    -- | Removes the user groups that can access this replication group.
    removeUserGroups :: Core.Maybe Core.Bool,
    -- | A description for the replication group. Maximum length is 255 characters.
    replicationGroupDescription :: Core.Maybe Types.String,
    -- | Specifies the VPC Security Groups associated with the clusters in the replication group.
    --
    -- This parameter can be used only with replication group containing clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Core.Maybe [Types.String],
    -- | The number of days for which ElastiCache retains automatic node group (shard) snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
    --
    -- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
    snapshotRetentionLimit :: Core.Maybe Core.Int,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of the node group (shard) specified by @SnapshottingClusterId@ .
    --
    -- Example: @05:00-09:00@
    -- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
    snapshotWindow :: Core.Maybe Types.String,
    -- | The cluster ID that is used as the daily snapshot source for the replication group. This parameter cannot be set for Redis (cluster mode enabled) replication groups.
    snapshottingClusterId :: Core.Maybe Types.String,
    -- | A list of user group IDs.
    userGroupIdsToAdd :: Core.Maybe [Types.UserGroupId],
    -- | A list of users groups to remove, meaning the users in the group no longer can access thereplication group.
    userGroupIdsToRemove :: Core.Maybe [Types.UserGroupId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyReplicationGroup' value with any optional fields omitted.
mkModifyReplicationGroup ::
  -- | 'replicationGroupId'
  Types.String ->
  ModifyReplicationGroup
mkModifyReplicationGroup replicationGroupId =
  ModifyReplicationGroup'
    { replicationGroupId,
      applyImmediately = Core.Nothing,
      authToken = Core.Nothing,
      authTokenUpdateStrategy = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      automaticFailoverEnabled = Core.Nothing,
      cacheNodeType = Core.Nothing,
      cacheParameterGroupName = Core.Nothing,
      cacheSecurityGroupNames = Core.Nothing,
      engineVersion = Core.Nothing,
      multiAZEnabled = Core.Nothing,
      nodeGroupId = Core.Nothing,
      notificationTopicArn = Core.Nothing,
      notificationTopicStatus = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      primaryClusterId = Core.Nothing,
      removeUserGroups = Core.Nothing,
      replicationGroupDescription = Core.Nothing,
      securityGroupIds = Core.Nothing,
      snapshotRetentionLimit = Core.Nothing,
      snapshotWindow = Core.Nothing,
      snapshottingClusterId = Core.Nothing,
      userGroupIdsToAdd = Core.Nothing,
      userGroupIdsToRemove = Core.Nothing
    }

-- | The identifier of the replication group to modify.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgReplicationGroupId :: Lens.Lens' ModifyReplicationGroup Types.String
mrgReplicationGroupId = Lens.field @"replicationGroupId"
{-# DEPRECATED mrgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

-- | If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the replication group.
--
-- If @false@ , changes to the nodes in the replication group are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
-- Valid values: @true@ | @false@
-- Default: @false@
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgApplyImmediately :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Core.Bool)
mrgApplyImmediately = Lens.field @"applyImmediately"
{-# DEPRECATED mrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Reserved parameter. The password used to access a password protected server. This parameter must be specified with the @auth-token-update-strategy @ parameter. Password constraints:
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
mrgAuthToken :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgAuthToken = Lens.field @"authToken"
{-# DEPRECATED mrgAuthToken "Use generic-lens or generic-optics with 'authToken' instead." #-}

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
mrgAuthTokenUpdateStrategy :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.AuthTokenUpdateStrategyType)
mrgAuthTokenUpdateStrategy = Lens.field @"authTokenUpdateStrategy"
{-# DEPRECATED mrgAuthTokenUpdateStrategy "Use generic-lens or generic-optics with 'authTokenUpdateStrategy' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgAutoMinorVersionUpgrade :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Core.Bool)
mrgAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# DEPRECATED mrgAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
--
-- /Note:/ Consider using 'automaticFailoverEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgAutomaticFailoverEnabled :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Core.Bool)
mrgAutomaticFailoverEnabled = Lens.field @"automaticFailoverEnabled"
{-# DEPRECATED mrgAutomaticFailoverEnabled "Use generic-lens or generic-optics with 'automaticFailoverEnabled' instead." #-}

-- | A valid cache node type that you want to scale this replication group to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgCacheNodeType :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgCacheNodeType = Lens.field @"cacheNodeType"
{-# DEPRECATED mrgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The name of the cache parameter group to apply to all of the clusters in this replication group. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgCacheParameterGroupName :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgCacheParameterGroupName = Lens.field @"cacheParameterGroupName"
{-# DEPRECATED mrgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | A list of cache security group names to authorize for the clusters in this replication group. This change is asynchronously applied as soon as possible.
--
-- This parameter can be used only with replication group containing clusters running outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be @Default@ .
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgCacheSecurityGroupNames :: Lens.Lens' ModifyReplicationGroup (Core.Maybe [Types.String])
mrgCacheSecurityGroupNames = Lens.field @"cacheSecurityGroupNames"
{-# DEPRECATED mrgCacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead." #-}

-- | The upgraded version of the cache engine to be run on the clusters in the replication group.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing replication group and create it anew with the earlier engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgEngineVersion :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED mrgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
--
-- /Note:/ Consider using 'multiAZEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgMultiAZEnabled :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Core.Bool)
mrgMultiAZEnabled = Lens.field @"multiAZEnabled"
{-# DEPRECATED mrgMultiAZEnabled "Use generic-lens or generic-optics with 'multiAZEnabled' instead." #-}

-- | Deprecated. This parameter is not used.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgNodeGroupId :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgNodeGroupId = Lens.field @"nodeGroupId"
{-# DEPRECATED mrgNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgNotificationTopicArn :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgNotificationTopicArn = Lens.field @"notificationTopicArn"
{-# DEPRECATED mrgNotificationTopicArn "Use generic-lens or generic-optics with 'notificationTopicArn' instead." #-}

-- | The status of the Amazon SNS notification topic for the replication group. Notifications are sent only if the status is @active@ .
--
-- Valid values: @active@ | @inactive@
--
-- /Note:/ Consider using 'notificationTopicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgNotificationTopicStatus :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgNotificationTopicStatus = Lens.field @"notificationTopicStatus"
{-# DEPRECATED mrgNotificationTopicStatus "Use generic-lens or generic-optics with 'notificationTopicStatus' instead." #-}

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
mrgPreferredMaintenanceWindow :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED mrgPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | For replication groups with a single primary, if this parameter is specified, ElastiCache promotes the specified cluster in the specified replication group to the primary role. The nodes of all other clusters in the replication group are read replicas.
--
-- /Note:/ Consider using 'primaryClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgPrimaryClusterId :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgPrimaryClusterId = Lens.field @"primaryClusterId"
{-# DEPRECATED mrgPrimaryClusterId "Use generic-lens or generic-optics with 'primaryClusterId' instead." #-}

-- | Removes the user groups that can access this replication group.
--
-- /Note:/ Consider using 'removeUserGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgRemoveUserGroups :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Core.Bool)
mrgRemoveUserGroups = Lens.field @"removeUserGroups"
{-# DEPRECATED mrgRemoveUserGroups "Use generic-lens or generic-optics with 'removeUserGroups' instead." #-}

-- | A description for the replication group. Maximum length is 255 characters.
--
-- /Note:/ Consider using 'replicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgReplicationGroupDescription :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgReplicationGroupDescription = Lens.field @"replicationGroupDescription"
{-# DEPRECATED mrgReplicationGroupDescription "Use generic-lens or generic-optics with 'replicationGroupDescription' instead." #-}

-- | Specifies the VPC Security Groups associated with the clusters in the replication group.
--
-- This parameter can be used only with replication group containing clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSecurityGroupIds :: Lens.Lens' ModifyReplicationGroup (Core.Maybe [Types.String])
mrgSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED mrgSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The number of days for which ElastiCache retains automatic node group (shard) snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSnapshotRetentionLimit :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Core.Int)
mrgSnapshotRetentionLimit = Lens.field @"snapshotRetentionLimit"
{-# DEPRECATED mrgSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of the node group (shard) specified by @SnapshottingClusterId@ .
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSnapshotWindow :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgSnapshotWindow = Lens.field @"snapshotWindow"
{-# DEPRECATED mrgSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

-- | The cluster ID that is used as the daily snapshot source for the replication group. This parameter cannot be set for Redis (cluster mode enabled) replication groups.
--
-- /Note:/ Consider using 'snapshottingClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSnapshottingClusterId :: Lens.Lens' ModifyReplicationGroup (Core.Maybe Types.String)
mrgSnapshottingClusterId = Lens.field @"snapshottingClusterId"
{-# DEPRECATED mrgSnapshottingClusterId "Use generic-lens or generic-optics with 'snapshottingClusterId' instead." #-}

-- | A list of user group IDs.
--
-- /Note:/ Consider using 'userGroupIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgUserGroupIdsToAdd :: Lens.Lens' ModifyReplicationGroup (Core.Maybe [Types.UserGroupId])
mrgUserGroupIdsToAdd = Lens.field @"userGroupIdsToAdd"
{-# DEPRECATED mrgUserGroupIdsToAdd "Use generic-lens or generic-optics with 'userGroupIdsToAdd' instead." #-}

-- | A list of users groups to remove, meaning the users in the group no longer can access thereplication group.
--
-- /Note:/ Consider using 'userGroupIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgUserGroupIdsToRemove :: Lens.Lens' ModifyReplicationGroup (Core.Maybe [Types.UserGroupId])
mrgUserGroupIdsToRemove = Lens.field @"userGroupIdsToRemove"
{-# DEPRECATED mrgUserGroupIdsToRemove "Use generic-lens or generic-optics with 'userGroupIdsToRemove' instead." #-}

instance Core.AWSRequest ModifyReplicationGroup where
  type Rs ModifyReplicationGroup = ModifyReplicationGroupResponse
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
            ( Core.pure ("Action", "ModifyReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> (Core.toQueryValue "ReplicationGroupId" replicationGroupId)
                Core.<> (Core.toQueryValue "ApplyImmediately" Core.<$> applyImmediately)
                Core.<> (Core.toQueryValue "AuthToken" Core.<$> authToken)
                Core.<> ( Core.toQueryValue "AuthTokenUpdateStrategy"
                            Core.<$> authTokenUpdateStrategy
                        )
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
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> (Core.toQueryValue "MultiAZEnabled" Core.<$> multiAZEnabled)
                Core.<> (Core.toQueryValue "NodeGroupId" Core.<$> nodeGroupId)
                Core.<> ( Core.toQueryValue "NotificationTopicArn"
                            Core.<$> notificationTopicArn
                        )
                Core.<> ( Core.toQueryValue "NotificationTopicStatus"
                            Core.<$> notificationTopicStatus
                        )
                Core.<> ( Core.toQueryValue "PreferredMaintenanceWindow"
                            Core.<$> preferredMaintenanceWindow
                        )
                Core.<> (Core.toQueryValue "PrimaryClusterId" Core.<$> primaryClusterId)
                Core.<> (Core.toQueryValue "RemoveUserGroups" Core.<$> removeUserGroups)
                Core.<> ( Core.toQueryValue "ReplicationGroupDescription"
                            Core.<$> replicationGroupDescription
                        )
                Core.<> ( Core.toQueryValue
                            "SecurityGroupIds"
                            (Core.toQueryList "SecurityGroupId" Core.<$> securityGroupIds)
                        )
                Core.<> ( Core.toQueryValue "SnapshotRetentionLimit"
                            Core.<$> snapshotRetentionLimit
                        )
                Core.<> (Core.toQueryValue "SnapshotWindow" Core.<$> snapshotWindow)
                Core.<> ( Core.toQueryValue "SnapshottingClusterId"
                            Core.<$> snapshottingClusterId
                        )
                Core.<> ( Core.toQueryValue
                            "UserGroupIdsToAdd"
                            (Core.toQueryList "member" Core.<$> userGroupIdsToAdd)
                        )
                Core.<> ( Core.toQueryValue
                            "UserGroupIdsToRemove"
                            (Core.toQueryList "member" Core.<$> userGroupIdsToRemove)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ModifyReplicationGroupResult"
      ( \s h x ->
          ModifyReplicationGroupResponse'
            Core.<$> (x Core..@? "ReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkModifyReplicationGroupResponse' smart constructor.
data ModifyReplicationGroupResponse = ModifyReplicationGroupResponse'
  { replicationGroup :: Core.Maybe Types.ReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ModifyReplicationGroupResponse' value with any optional fields omitted.
mkModifyReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ModifyReplicationGroupResponse
mkModifyReplicationGroupResponse responseStatus =
  ModifyReplicationGroupResponse'
    { replicationGroup = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgrrsReplicationGroup :: Lens.Lens' ModifyReplicationGroupResponse (Core.Maybe Types.ReplicationGroup)
mrgrrsReplicationGroup = Lens.field @"replicationGroup"
{-# DEPRECATED mrgrrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgrrsResponseStatus :: Lens.Lens' ModifyReplicationGroupResponse Core.Int
mrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED mrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
