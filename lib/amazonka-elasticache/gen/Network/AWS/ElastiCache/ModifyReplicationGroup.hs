{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    mrgAutomaticFailoverEnabled,
    mrgEngineVersion,
    mrgCacheNodeType,
    mrgSnapshottingClusterId,
    mrgSecurityGroupIds,
    mrgAutoMinorVersionUpgrade,
    mrgCacheParameterGroupName,
    mrgReplicationGroupDescription,
    mrgSnapshotWindow,
    mrgAuthToken,
    mrgPrimaryClusterId,
    mrgPreferredMaintenanceWindow,
    mrgMultiAZEnabled,
    mrgUserGroupIdsToAdd,
    mrgNodeGroupId,
    mrgSnapshotRetentionLimit,
    mrgUserGroupIdsToRemove,
    mrgNotificationTopicStatus,
    mrgApplyImmediately,
    mrgRemoveUserGroups,
    mrgAuthTokenUpdateStrategy,
    mrgNotificationTopicARN,
    mrgCacheSecurityGroupNames,
    mrgReplicationGroupId,

    -- * Destructuring the response
    ModifyReplicationGroupResponse (..),
    mkModifyReplicationGroupResponse,

    -- ** Response lenses
    mrgrsReplicationGroup,
    mrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @ModifyReplicationGroups@ operation.
--
-- /See:/ 'mkModifyReplicationGroup' smart constructor.
data ModifyReplicationGroup = ModifyReplicationGroup'
  { automaticFailoverEnabled ::
      Lude.Maybe Lude.Bool,
    engineVersion :: Lude.Maybe Lude.Text,
    cacheNodeType :: Lude.Maybe Lude.Text,
    snapshottingClusterId :: Lude.Maybe Lude.Text,
    securityGroupIds :: Lude.Maybe [Lude.Text],
    autoMinorVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    cacheParameterGroupName ::
      Lude.Maybe Lude.Text,
    replicationGroupDescription ::
      Lude.Maybe Lude.Text,
    snapshotWindow :: Lude.Maybe Lude.Text,
    authToken :: Lude.Maybe Lude.Text,
    primaryClusterId :: Lude.Maybe Lude.Text,
    preferredMaintenanceWindow ::
      Lude.Maybe Lude.Text,
    multiAZEnabled :: Lude.Maybe Lude.Bool,
    userGroupIdsToAdd :: Lude.Maybe [Lude.Text],
    nodeGroupId :: Lude.Maybe Lude.Text,
    snapshotRetentionLimit :: Lude.Maybe Lude.Int,
    userGroupIdsToRemove ::
      Lude.Maybe [Lude.Text],
    notificationTopicStatus ::
      Lude.Maybe Lude.Text,
    applyImmediately :: Lude.Maybe Lude.Bool,
    removeUserGroups :: Lude.Maybe Lude.Bool,
    authTokenUpdateStrategy ::
      Lude.Maybe AuthTokenUpdateStrategyType,
    notificationTopicARN :: Lude.Maybe Lude.Text,
    cacheSecurityGroupNames ::
      Lude.Maybe [Lude.Text],
    replicationGroupId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReplicationGroup' with the minimum fields required to make a request.
--
-- * 'applyImmediately' - If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the replication group.
--
-- If @false@ , changes to the nodes in the replication group are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
-- Valid values: @true@ | @false@
-- Default: @false@
-- * 'authToken' - Reserved parameter. The password used to access a password protected server. This parameter must be specified with the @auth-token-update-strategy @ parameter. Password constraints:
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
-- * 'autoMinorVersionUpgrade' - This parameter is currently disabled.
-- * 'automaticFailoverEnabled' - Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
-- * 'cacheNodeType' - A valid cache node type that you want to scale this replication group to.
-- * 'cacheParameterGroupName' - The name of the cache parameter group to apply to all of the clusters in this replication group. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
-- * 'cacheSecurityGroupNames' - A list of cache security group names to authorize for the clusters in this replication group. This change is asynchronously applied as soon as possible.
--
-- This parameter can be used only with replication group containing clusters running outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be @Default@ .
-- * 'engineVersion' - The upgraded version of the cache engine to be run on the clusters in the replication group.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing replication group and create it anew with the earlier engine version.
-- * 'multiAZEnabled' - A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
-- * 'nodeGroupId' - Deprecated. This parameter is not used.
-- * 'notificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
-- * 'notificationTopicStatus' - The status of the Amazon SNS notification topic for the replication group. Notifications are sent only if the status is @active@ .
--
-- Valid values: @active@ | @inactive@
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
-- * 'primaryClusterId' - For replication groups with a single primary, if this parameter is specified, ElastiCache promotes the specified cluster in the specified replication group to the primary role. The nodes of all other clusters in the replication group are read replicas.
-- * 'removeUserGroups' - Removes the user groups that can access this replication group.
-- * 'replicationGroupDescription' - A description for the replication group. Maximum length is 255 characters.
-- * 'replicationGroupId' - The identifier of the replication group to modify.
-- * 'securityGroupIds' - Specifies the VPC Security Groups associated with the clusters in the replication group.
--
-- This parameter can be used only with replication group containing clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
-- * 'snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic node group (shard) snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
-- * 'snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of the node group (shard) specified by @SnapshottingClusterId@ .
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
-- * 'snapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the replication group. This parameter cannot be set for Redis (cluster mode enabled) replication groups.
-- * 'userGroupIdsToAdd' - A list of user group IDs.
-- * 'userGroupIdsToRemove' - A list of users groups to remove, meaning the users in the group no longer can access thereplication group.
mkModifyReplicationGroup ::
  -- | 'replicationGroupId'
  Lude.Text ->
  ModifyReplicationGroup
mkModifyReplicationGroup pReplicationGroupId_ =
  ModifyReplicationGroup'
    { automaticFailoverEnabled = Lude.Nothing,
      engineVersion = Lude.Nothing,
      cacheNodeType = Lude.Nothing,
      snapshottingClusterId = Lude.Nothing,
      securityGroupIds = Lude.Nothing,
      autoMinorVersionUpgrade = Lude.Nothing,
      cacheParameterGroupName = Lude.Nothing,
      replicationGroupDescription = Lude.Nothing,
      snapshotWindow = Lude.Nothing,
      authToken = Lude.Nothing,
      primaryClusterId = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      multiAZEnabled = Lude.Nothing,
      userGroupIdsToAdd = Lude.Nothing,
      nodeGroupId = Lude.Nothing,
      snapshotRetentionLimit = Lude.Nothing,
      userGroupIdsToRemove = Lude.Nothing,
      notificationTopicStatus = Lude.Nothing,
      applyImmediately = Lude.Nothing,
      removeUserGroups = Lude.Nothing,
      authTokenUpdateStrategy = Lude.Nothing,
      notificationTopicARN = Lude.Nothing,
      cacheSecurityGroupNames = Lude.Nothing,
      replicationGroupId = pReplicationGroupId_
    }

-- | Determines whether a read replica is automatically promoted to read/write primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
--
-- /Note:/ Consider using 'automaticFailoverEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgAutomaticFailoverEnabled :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Bool)
mrgAutomaticFailoverEnabled = Lens.lens (automaticFailoverEnabled :: ModifyReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {automaticFailoverEnabled = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgAutomaticFailoverEnabled "Use generic-lens or generic-optics with 'automaticFailoverEnabled' instead." #-}

-- | The upgraded version of the cache engine to be run on the clusters in the replication group.
--
-- __Important:__ You can upgrade to a newer engine version (see <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version> ), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing replication group and create it anew with the earlier engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgEngineVersion :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgEngineVersion = Lens.lens (engineVersion :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | A valid cache node type that you want to scale this replication group to.
--
-- /Note:/ Consider using 'cacheNodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgCacheNodeType :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgCacheNodeType = Lens.lens (cacheNodeType :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeType = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgCacheNodeType "Use generic-lens or generic-optics with 'cacheNodeType' instead." #-}

-- | The cluster ID that is used as the daily snapshot source for the replication group. This parameter cannot be set for Redis (cluster mode enabled) replication groups.
--
-- /Note:/ Consider using 'snapshottingClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSnapshottingClusterId :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgSnapshottingClusterId = Lens.lens (snapshottingClusterId :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {snapshottingClusterId = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgSnapshottingClusterId "Use generic-lens or generic-optics with 'snapshottingClusterId' instead." #-}

-- | Specifies the VPC Security Groups associated with the clusters in the replication group.
--
-- This parameter can be used only with replication group containing clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSecurityGroupIds :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe [Lude.Text])
mrgSecurityGroupIds = Lens.lens (securityGroupIds :: ModifyReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | This parameter is currently disabled.
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgAutoMinorVersionUpgrade :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Bool)
mrgAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: ModifyReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

-- | The name of the cache parameter group to apply to all of the clusters in this replication group. This change is asynchronously applied as soon as possible for parameters when the @ApplyImmediately@ parameter is specified as @true@ for this request.
--
-- /Note:/ Consider using 'cacheParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgCacheParameterGroupName :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgCacheParameterGroupName = Lens.lens (cacheParameterGroupName :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheParameterGroupName = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgCacheParameterGroupName "Use generic-lens or generic-optics with 'cacheParameterGroupName' instead." #-}

-- | A description for the replication group. Maximum length is 255 characters.
--
-- /Note:/ Consider using 'replicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgReplicationGroupDescription :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgReplicationGroupDescription = Lens.lens (replicationGroupDescription :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {replicationGroupDescription = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgReplicationGroupDescription "Use generic-lens or generic-optics with 'replicationGroupDescription' instead." #-}

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of the node group (shard) specified by @SnapshottingClusterId@ .
--
-- Example: @05:00-09:00@
-- If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- /Note:/ Consider using 'snapshotWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSnapshotWindow :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgSnapshotWindow = Lens.lens (snapshotWindow :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {snapshotWindow = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgSnapshotWindow "Use generic-lens or generic-optics with 'snapshotWindow' instead." #-}

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
mrgAuthToken :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgAuthToken = Lens.lens (authToken :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {authToken = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgAuthToken "Use generic-lens or generic-optics with 'authToken' instead." #-}

-- | For replication groups with a single primary, if this parameter is specified, ElastiCache promotes the specified cluster in the specified replication group to the primary role. The nodes of all other clusters in the replication group are read replicas.
--
-- /Note:/ Consider using 'primaryClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgPrimaryClusterId :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgPrimaryClusterId = Lens.lens (primaryClusterId :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {primaryClusterId = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgPrimaryClusterId "Use generic-lens or generic-optics with 'primaryClusterId' instead." #-}

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
mrgPreferredMaintenanceWindow :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | A flag indicating if you have Multi-AZ enabled to enhance fault tolerance. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ> .
--
-- /Note:/ Consider using 'multiAZEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgMultiAZEnabled :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Bool)
mrgMultiAZEnabled = Lens.lens (multiAZEnabled :: ModifyReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZEnabled = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgMultiAZEnabled "Use generic-lens or generic-optics with 'multiAZEnabled' instead." #-}

-- | A list of user group IDs.
--
-- /Note:/ Consider using 'userGroupIdsToAdd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgUserGroupIdsToAdd :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe [Lude.Text])
mrgUserGroupIdsToAdd = Lens.lens (userGroupIdsToAdd :: ModifyReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroupIdsToAdd = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgUserGroupIdsToAdd "Use generic-lens or generic-optics with 'userGroupIdsToAdd' instead." #-}

-- | Deprecated. This parameter is not used.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgNodeGroupId :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgNodeGroupId = Lens.lens (nodeGroupId :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {nodeGroupId = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The number of days for which ElastiCache retains automatic node group (shard) snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- /Note:/ Consider using 'snapshotRetentionLimit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgSnapshotRetentionLimit :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Int)
mrgSnapshotRetentionLimit = Lens.lens (snapshotRetentionLimit :: ModifyReplicationGroup -> Lude.Maybe Lude.Int) (\s a -> s {snapshotRetentionLimit = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgSnapshotRetentionLimit "Use generic-lens or generic-optics with 'snapshotRetentionLimit' instead." #-}

-- | A list of users groups to remove, meaning the users in the group no longer can access thereplication group.
--
-- /Note:/ Consider using 'userGroupIdsToRemove' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgUserGroupIdsToRemove :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe [Lude.Text])
mrgUserGroupIdsToRemove = Lens.lens (userGroupIdsToRemove :: ModifyReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {userGroupIdsToRemove = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgUserGroupIdsToRemove "Use generic-lens or generic-optics with 'userGroupIdsToRemove' instead." #-}

-- | The status of the Amazon SNS notification topic for the replication group. Notifications are sent only if the status is @active@ .
--
-- Valid values: @active@ | @inactive@
--
-- /Note:/ Consider using 'notificationTopicStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgNotificationTopicStatus :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgNotificationTopicStatus = Lens.lens (notificationTopicStatus :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicStatus = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgNotificationTopicStatus "Use generic-lens or generic-optics with 'notificationTopicStatus' instead." #-}

-- | If @true@ , this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the @PreferredMaintenanceWindow@ setting for the replication group.
--
-- If @false@ , changes to the nodes in the replication group are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
-- Valid values: @true@ | @false@
-- Default: @false@
--
-- /Note:/ Consider using 'applyImmediately' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgApplyImmediately :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Bool)
mrgApplyImmediately = Lens.lens (applyImmediately :: ModifyReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {applyImmediately = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgApplyImmediately "Use generic-lens or generic-optics with 'applyImmediately' instead." #-}

-- | Removes the user groups that can access this replication group.
--
-- /Note:/ Consider using 'removeUserGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgRemoveUserGroups :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Bool)
mrgRemoveUserGroups = Lens.lens (removeUserGroups :: ModifyReplicationGroup -> Lude.Maybe Lude.Bool) (\s a -> s {removeUserGroups = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgRemoveUserGroups "Use generic-lens or generic-optics with 'removeUserGroups' instead." #-}

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
mrgAuthTokenUpdateStrategy :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe AuthTokenUpdateStrategyType)
mrgAuthTokenUpdateStrategy = Lens.lens (authTokenUpdateStrategy :: ModifyReplicationGroup -> Lude.Maybe AuthTokenUpdateStrategyType) (\s a -> s {authTokenUpdateStrategy = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgAuthTokenUpdateStrategy "Use generic-lens or generic-optics with 'authTokenUpdateStrategy' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications are sent.
--
-- /Note:/ Consider using 'notificationTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgNotificationTopicARN :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe Lude.Text)
mrgNotificationTopicARN = Lens.lens (notificationTopicARN :: ModifyReplicationGroup -> Lude.Maybe Lude.Text) (\s a -> s {notificationTopicARN = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgNotificationTopicARN "Use generic-lens or generic-optics with 'notificationTopicARN' instead." #-}

-- | A list of cache security group names to authorize for the clusters in this replication group. This change is asynchronously applied as soon as possible.
--
-- This parameter can be used only with replication group containing clusters running outside of an Amazon Virtual Private Cloud (Amazon VPC).
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be @Default@ .
--
-- /Note:/ Consider using 'cacheSecurityGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgCacheSecurityGroupNames :: Lens.Lens' ModifyReplicationGroup (Lude.Maybe [Lude.Text])
mrgCacheSecurityGroupNames = Lens.lens (cacheSecurityGroupNames :: ModifyReplicationGroup -> Lude.Maybe [Lude.Text]) (\s a -> s {cacheSecurityGroupNames = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgCacheSecurityGroupNames "Use generic-lens or generic-optics with 'cacheSecurityGroupNames' instead." #-}

-- | The identifier of the replication group to modify.
--
-- /Note:/ Consider using 'replicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgReplicationGroupId :: Lens.Lens' ModifyReplicationGroup Lude.Text
mrgReplicationGroupId = Lens.lens (replicationGroupId :: ModifyReplicationGroup -> Lude.Text) (\s a -> s {replicationGroupId = a} :: ModifyReplicationGroup)
{-# DEPRECATED mrgReplicationGroupId "Use generic-lens or generic-optics with 'replicationGroupId' instead." #-}

instance Lude.AWSRequest ModifyReplicationGroup where
  type Rs ModifyReplicationGroup = ModifyReplicationGroupResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "ModifyReplicationGroupResult"
      ( \s h x ->
          ModifyReplicationGroupResponse'
            Lude.<$> (x Lude..@? "ReplicationGroup")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyReplicationGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyReplicationGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyReplicationGroup where
  toQuery ModifyReplicationGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyReplicationGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "AutomaticFailoverEnabled" Lude.=: automaticFailoverEnabled,
        "EngineVersion" Lude.=: engineVersion,
        "CacheNodeType" Lude.=: cacheNodeType,
        "SnapshottingClusterId" Lude.=: snapshottingClusterId,
        "SecurityGroupIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "SecurityGroupId" Lude.<$> securityGroupIds),
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "CacheParameterGroupName" Lude.=: cacheParameterGroupName,
        "ReplicationGroupDescription" Lude.=: replicationGroupDescription,
        "SnapshotWindow" Lude.=: snapshotWindow,
        "AuthToken" Lude.=: authToken,
        "PrimaryClusterId" Lude.=: primaryClusterId,
        "PreferredMaintenanceWindow" Lude.=: preferredMaintenanceWindow,
        "MultiAZEnabled" Lude.=: multiAZEnabled,
        "UserGroupIdsToAdd"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> userGroupIdsToAdd),
        "NodeGroupId" Lude.=: nodeGroupId,
        "SnapshotRetentionLimit" Lude.=: snapshotRetentionLimit,
        "UserGroupIdsToRemove"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> userGroupIdsToRemove),
        "NotificationTopicStatus" Lude.=: notificationTopicStatus,
        "ApplyImmediately" Lude.=: applyImmediately,
        "RemoveUserGroups" Lude.=: removeUserGroups,
        "AuthTokenUpdateStrategy" Lude.=: authTokenUpdateStrategy,
        "NotificationTopicArn" Lude.=: notificationTopicARN,
        "CacheSecurityGroupNames"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "CacheSecurityGroupName"
                Lude.<$> cacheSecurityGroupNames
            ),
        "ReplicationGroupId" Lude.=: replicationGroupId
      ]

-- | /See:/ 'mkModifyReplicationGroupResponse' smart constructor.
data ModifyReplicationGroupResponse = ModifyReplicationGroupResponse'
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

-- | Creates a value of 'ModifyReplicationGroupResponse' with the minimum fields required to make a request.
--
-- * 'replicationGroup' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyReplicationGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyReplicationGroupResponse
mkModifyReplicationGroupResponse pResponseStatus_ =
  ModifyReplicationGroupResponse'
    { replicationGroup = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgrsReplicationGroup :: Lens.Lens' ModifyReplicationGroupResponse (Lude.Maybe ReplicationGroup)
mrgrsReplicationGroup = Lens.lens (replicationGroup :: ModifyReplicationGroupResponse -> Lude.Maybe ReplicationGroup) (\s a -> s {replicationGroup = a} :: ModifyReplicationGroupResponse)
{-# DEPRECATED mrgrsReplicationGroup "Use generic-lens or generic-optics with 'replicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrgrsResponseStatus :: Lens.Lens' ModifyReplicationGroupResponse Lude.Int
mrgrsResponseStatus = Lens.lens (responseStatus :: ModifyReplicationGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyReplicationGroupResponse)
{-# DEPRECATED mrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
