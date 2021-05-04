{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a replication group.
--
-- -   <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/scaling-redis-cluster-mode-enabled.html Scaling for Amazon ElastiCache for Redis (cluster mode enabled)>
--     in the ElastiCache User Guide
--
-- -   <https://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyReplicationGroupShardConfiguration.html ModifyReplicationGroupShardConfiguration>
--     in the ElastiCache API Reference
--
-- This operation is valid for Redis only.
module Network.AWS.ElastiCache.ModifyReplicationGroup
  ( -- * Creating a Request
    ModifyReplicationGroup (..),
    newModifyReplicationGroup,

    -- * Request Lenses
    modifyReplicationGroup_securityGroupIds,
    modifyReplicationGroup_automaticFailoverEnabled,
    modifyReplicationGroup_authTokenUpdateStrategy,
    modifyReplicationGroup_cacheSecurityGroupNames,
    modifyReplicationGroup_primaryClusterId,
    modifyReplicationGroup_snapshotWindow,
    modifyReplicationGroup_notificationTopicStatus,
    modifyReplicationGroup_userGroupIdsToRemove,
    modifyReplicationGroup_replicationGroupDescription,
    modifyReplicationGroup_cacheParameterGroupName,
    modifyReplicationGroup_snapshotRetentionLimit,
    modifyReplicationGroup_nodeGroupId,
    modifyReplicationGroup_userGroupIdsToAdd,
    modifyReplicationGroup_multiAZEnabled,
    modifyReplicationGroup_snapshottingClusterId,
    modifyReplicationGroup_engineVersion,
    modifyReplicationGroup_preferredMaintenanceWindow,
    modifyReplicationGroup_cacheNodeType,
    modifyReplicationGroup_notificationTopicArn,
    modifyReplicationGroup_authToken,
    modifyReplicationGroup_removeUserGroups,
    modifyReplicationGroup_applyImmediately,
    modifyReplicationGroup_autoMinorVersionUpgrade,
    modifyReplicationGroup_replicationGroupId,

    -- * Destructuring the Response
    ModifyReplicationGroupResponse (..),
    newModifyReplicationGroupResponse,

    -- * Response Lenses
    modifyReplicationGroupResponse_replicationGroup,
    modifyReplicationGroupResponse_httpStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ModifyReplicationGroups@ operation.
--
-- /See:/ 'newModifyReplicationGroup' smart constructor.
data ModifyReplicationGroup = ModifyReplicationGroup'
  { -- | Specifies the VPC Security Groups associated with the clusters in the
    -- replication group.
    --
    -- This parameter can be used only with replication group containing
    -- clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Determines whether a read replica is automatically promoted to
    -- read\/write primary if the existing primary encounters a failure.
    --
    -- Valid values: @true@ | @false@
    automaticFailoverEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the strategy to use to update the AUTH token. This parameter
    -- must be specified with the @auth-token@ parameter. Possible values:
    --
    -- -   Rotate
    --
    -- -   Set
    --
    -- For more information, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
    authTokenUpdateStrategy :: Prelude.Maybe AuthTokenUpdateStrategyType,
    -- | A list of cache security group names to authorize for the clusters in
    -- this replication group. This change is asynchronously applied as soon as
    -- possible.
    --
    -- This parameter can be used only with replication group containing
    -- clusters running outside of an Amazon Virtual Private Cloud (Amazon
    -- VPC).
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters. Must
    -- not be @Default@.
    cacheSecurityGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | For replication groups with a single primary, if this parameter is
    -- specified, ElastiCache promotes the specified cluster in the specified
    -- replication group to the primary role. The nodes of all other clusters
    -- in the replication group are read replicas.
    primaryClusterId :: Prelude.Maybe Prelude.Text,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of the node group (shard) specified by
    -- @SnapshottingClusterId@.
    --
    -- Example: @05:00-09:00@
    --
    -- If you do not specify this parameter, ElastiCache automatically chooses
    -- an appropriate time range.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The status of the Amazon SNS notification topic for the replication
    -- group. Notifications are sent only if the status is @active@.
    --
    -- Valid values: @active@ | @inactive@
    notificationTopicStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of users groups to remove, meaning the users in the group no
    -- longer can access thereplication group.
    userGroupIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | A description for the replication group. Maximum length is 255
    -- characters.
    replicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache parameter group to apply to all of the clusters in
    -- this replication group. This change is asynchronously applied as soon as
    -- possible for parameters when the @ApplyImmediately@ parameter is
    -- specified as @true@ for this request.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which ElastiCache retains automatic node group
    -- (shard) snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
    -- backups are turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | Deprecated. This parameter is not used.
    nodeGroupId :: Prelude.Maybe Prelude.Text,
    -- | A list of user group IDs.
    userGroupIdsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault
    -- tolerance. For more information, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>.
    multiAZEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The cluster ID that is used as the daily snapshot source for the
    -- replication group. This parameter cannot be set for Redis (cluster mode
    -- enabled) replication groups.
    snapshottingClusterId :: Prelude.Maybe Prelude.Text,
    -- | The upgraded version of the cache engine to be run on the clusters in
    -- the replication group.
    --
    -- __Important:__ You can upgrade to a newer engine version (see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
    -- but you cannot downgrade to an earlier engine version. If you want to
    -- use an earlier engine version, you must delete the existing replication
    -- group and create it anew with the earlier engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period.
    --
    -- Valid values for @ddd@ are:
    --
    -- -   @sun@
    --
    -- -   @mon@
    --
    -- -   @tue@
    --
    -- -   @wed@
    --
    -- -   @thu@
    --
    -- -   @fri@
    --
    -- -   @sat@
    --
    -- Example: @sun:23:00-mon:01:30@
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A valid cache node type that you want to scale this replication group
    -- to.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
    -- notifications are sent.
    --
    -- The Amazon SNS topic owner must be same as the replication group owner.
    notificationTopicArn :: Prelude.Maybe Prelude.Text,
    -- | Reserved parameter. The password used to access a password protected
    -- server. This parameter must be specified with the
    -- @auth-token-update-strategy @ parameter. Password constraints:
    --
    -- -   Must be only printable ASCII characters
    --
    -- -   Must be at least 16 characters and no more than 128 characters in
    --     length
    --
    -- -   Cannot contain any of the following characters: \'\/\', \'\"\', or
    --     \'\@\', \'%\'
    --
    -- For more information, see AUTH password at
    -- <http://redis.io/commands/AUTH AUTH>.
    authToken :: Prelude.Maybe Prelude.Text,
    -- | Removes the user groups that can access this replication group.
    removeUserGroups :: Prelude.Maybe Prelude.Bool,
    -- | If @true@, this parameter causes the modifications in this request and
    -- any pending modifications to be applied, asynchronously and as soon as
    -- possible, regardless of the @PreferredMaintenanceWindow@ setting for the
    -- replication group.
    --
    -- If @false@, changes to the nodes in the replication group are applied on
    -- the next maintenance reboot, or the next failure reboot, whichever
    -- occurs first.
    --
    -- Valid values: @true@ | @false@
    --
    -- Default: @false@
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the replication group to modify.
    replicationGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'modifyReplicationGroup_securityGroupIds' - Specifies the VPC Security Groups associated with the clusters in the
-- replication group.
--
-- This parameter can be used only with replication group containing
-- clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- 'automaticFailoverEnabled', 'modifyReplicationGroup_automaticFailoverEnabled' - Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
--
-- 'authTokenUpdateStrategy', 'modifyReplicationGroup_authTokenUpdateStrategy' - Specifies the strategy to use to update the AUTH token. This parameter
-- must be specified with the @auth-token@ parameter. Possible values:
--
-- -   Rotate
--
-- -   Set
--
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
--
-- 'cacheSecurityGroupNames', 'modifyReplicationGroup_cacheSecurityGroupNames' - A list of cache security group names to authorize for the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible.
--
-- This parameter can be used only with replication group containing
-- clusters running outside of an Amazon Virtual Private Cloud (Amazon
-- VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must
-- not be @Default@.
--
-- 'primaryClusterId', 'modifyReplicationGroup_primaryClusterId' - For replication groups with a single primary, if this parameter is
-- specified, ElastiCache promotes the specified cluster in the specified
-- replication group to the primary role. The nodes of all other clusters
-- in the replication group are read replicas.
--
-- 'snapshotWindow', 'modifyReplicationGroup_snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of the node group (shard) specified by
-- @SnapshottingClusterId@.
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
--
-- 'notificationTopicStatus', 'modifyReplicationGroup_notificationTopicStatus' - The status of the Amazon SNS notification topic for the replication
-- group. Notifications are sent only if the status is @active@.
--
-- Valid values: @active@ | @inactive@
--
-- 'userGroupIdsToRemove', 'modifyReplicationGroup_userGroupIdsToRemove' - A list of users groups to remove, meaning the users in the group no
-- longer can access thereplication group.
--
-- 'replicationGroupDescription', 'modifyReplicationGroup_replicationGroupDescription' - A description for the replication group. Maximum length is 255
-- characters.
--
-- 'cacheParameterGroupName', 'modifyReplicationGroup_cacheParameterGroupName' - The name of the cache parameter group to apply to all of the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the @ApplyImmediately@ parameter is
-- specified as @true@ for this request.
--
-- 'snapshotRetentionLimit', 'modifyReplicationGroup_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic node group
-- (shard) snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
-- backups are turned off.
--
-- 'nodeGroupId', 'modifyReplicationGroup_nodeGroupId' - Deprecated. This parameter is not used.
--
-- 'userGroupIdsToAdd', 'modifyReplicationGroup_userGroupIdsToAdd' - A list of user group IDs.
--
-- 'multiAZEnabled', 'modifyReplicationGroup_multiAZEnabled' - A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>.
--
-- 'snapshottingClusterId', 'modifyReplicationGroup_snapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the
-- replication group. This parameter cannot be set for Redis (cluster mode
-- enabled) replication groups.
--
-- 'engineVersion', 'modifyReplicationGroup_engineVersion' - The upgraded version of the cache engine to be run on the clusters in
-- the replication group.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
-- but you cannot downgrade to an earlier engine version. If you want to
-- use an earlier engine version, you must delete the existing replication
-- group and create it anew with the earlier engine version.
--
-- 'preferredMaintenanceWindow', 'modifyReplicationGroup_preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
--
-- 'cacheNodeType', 'modifyReplicationGroup_cacheNodeType' - A valid cache node type that you want to scale this replication group
-- to.
--
-- 'notificationTopicArn', 'modifyReplicationGroup_notificationTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications are sent.
--
-- The Amazon SNS topic owner must be same as the replication group owner.
--
-- 'authToken', 'modifyReplicationGroup_authToken' - Reserved parameter. The password used to access a password protected
-- server. This parameter must be specified with the
-- @auth-token-update-strategy @ parameter. Password constraints:
--
-- -   Must be only printable ASCII characters
--
-- -   Must be at least 16 characters and no more than 128 characters in
--     length
--
-- -   Cannot contain any of the following characters: \'\/\', \'\"\', or
--     \'\@\', \'%\'
--
-- For more information, see AUTH password at
-- <http://redis.io/commands/AUTH AUTH>.
--
-- 'removeUserGroups', 'modifyReplicationGroup_removeUserGroups' - Removes the user groups that can access this replication group.
--
-- 'applyImmediately', 'modifyReplicationGroup_applyImmediately' - If @true@, this parameter causes the modifications in this request and
-- any pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the @PreferredMaintenanceWindow@ setting for the
-- replication group.
--
-- If @false@, changes to the nodes in the replication group are applied on
-- the next maintenance reboot, or the next failure reboot, whichever
-- occurs first.
--
-- Valid values: @true@ | @false@
--
-- Default: @false@
--
-- 'autoMinorVersionUpgrade', 'modifyReplicationGroup_autoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- 'replicationGroupId', 'modifyReplicationGroup_replicationGroupId' - The identifier of the replication group to modify.
newModifyReplicationGroup ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  ModifyReplicationGroup
newModifyReplicationGroup pReplicationGroupId_ =
  ModifyReplicationGroup'
    { securityGroupIds =
        Prelude.Nothing,
      automaticFailoverEnabled = Prelude.Nothing,
      authTokenUpdateStrategy = Prelude.Nothing,
      cacheSecurityGroupNames = Prelude.Nothing,
      primaryClusterId = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      notificationTopicStatus = Prelude.Nothing,
      userGroupIdsToRemove = Prelude.Nothing,
      replicationGroupDescription = Prelude.Nothing,
      cacheParameterGroupName = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      nodeGroupId = Prelude.Nothing,
      userGroupIdsToAdd = Prelude.Nothing,
      multiAZEnabled = Prelude.Nothing,
      snapshottingClusterId = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      notificationTopicArn = Prelude.Nothing,
      authToken = Prelude.Nothing,
      removeUserGroups = Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      replicationGroupId = pReplicationGroupId_
    }

-- | Specifies the VPC Security Groups associated with the clusters in the
-- replication group.
--
-- This parameter can be used only with replication group containing
-- clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
modifyReplicationGroup_securityGroupIds :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [Prelude.Text])
modifyReplicationGroup_securityGroupIds = Lens.lens (\ModifyReplicationGroup' {securityGroupIds} -> securityGroupIds) (\s@ModifyReplicationGroup' {} a -> s {securityGroupIds = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
modifyReplicationGroup_automaticFailoverEnabled :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_automaticFailoverEnabled = Lens.lens (\ModifyReplicationGroup' {automaticFailoverEnabled} -> automaticFailoverEnabled) (\s@ModifyReplicationGroup' {} a -> s {automaticFailoverEnabled = a} :: ModifyReplicationGroup)

-- | Specifies the strategy to use to update the AUTH token. This parameter
-- must be specified with the @auth-token@ parameter. Possible values:
--
-- -   Rotate
--
-- -   Set
--
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
modifyReplicationGroup_authTokenUpdateStrategy :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe AuthTokenUpdateStrategyType)
modifyReplicationGroup_authTokenUpdateStrategy = Lens.lens (\ModifyReplicationGroup' {authTokenUpdateStrategy} -> authTokenUpdateStrategy) (\s@ModifyReplicationGroup' {} a -> s {authTokenUpdateStrategy = a} :: ModifyReplicationGroup)

-- | A list of cache security group names to authorize for the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible.
--
-- This parameter can be used only with replication group containing
-- clusters running outside of an Amazon Virtual Private Cloud (Amazon
-- VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must
-- not be @Default@.
modifyReplicationGroup_cacheSecurityGroupNames :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [Prelude.Text])
modifyReplicationGroup_cacheSecurityGroupNames = Lens.lens (\ModifyReplicationGroup' {cacheSecurityGroupNames} -> cacheSecurityGroupNames) (\s@ModifyReplicationGroup' {} a -> s {cacheSecurityGroupNames = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | For replication groups with a single primary, if this parameter is
-- specified, ElastiCache promotes the specified cluster in the specified
-- replication group to the primary role. The nodes of all other clusters
-- in the replication group are read replicas.
modifyReplicationGroup_primaryClusterId :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_primaryClusterId = Lens.lens (\ModifyReplicationGroup' {primaryClusterId} -> primaryClusterId) (\s@ModifyReplicationGroup' {} a -> s {primaryClusterId = a} :: ModifyReplicationGroup)

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of the node group (shard) specified by
-- @SnapshottingClusterId@.
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
modifyReplicationGroup_snapshotWindow :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_snapshotWindow = Lens.lens (\ModifyReplicationGroup' {snapshotWindow} -> snapshotWindow) (\s@ModifyReplicationGroup' {} a -> s {snapshotWindow = a} :: ModifyReplicationGroup)

-- | The status of the Amazon SNS notification topic for the replication
-- group. Notifications are sent only if the status is @active@.
--
-- Valid values: @active@ | @inactive@
modifyReplicationGroup_notificationTopicStatus :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_notificationTopicStatus = Lens.lens (\ModifyReplicationGroup' {notificationTopicStatus} -> notificationTopicStatus) (\s@ModifyReplicationGroup' {} a -> s {notificationTopicStatus = a} :: ModifyReplicationGroup)

-- | A list of users groups to remove, meaning the users in the group no
-- longer can access thereplication group.
modifyReplicationGroup_userGroupIdsToRemove :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [Prelude.Text])
modifyReplicationGroup_userGroupIdsToRemove = Lens.lens (\ModifyReplicationGroup' {userGroupIdsToRemove} -> userGroupIdsToRemove) (\s@ModifyReplicationGroup' {} a -> s {userGroupIdsToRemove = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | A description for the replication group. Maximum length is 255
-- characters.
modifyReplicationGroup_replicationGroupDescription :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_replicationGroupDescription = Lens.lens (\ModifyReplicationGroup' {replicationGroupDescription} -> replicationGroupDescription) (\s@ModifyReplicationGroup' {} a -> s {replicationGroupDescription = a} :: ModifyReplicationGroup)

-- | The name of the cache parameter group to apply to all of the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the @ApplyImmediately@ parameter is
-- specified as @true@ for this request.
modifyReplicationGroup_cacheParameterGroupName :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_cacheParameterGroupName = Lens.lens (\ModifyReplicationGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ModifyReplicationGroup' {} a -> s {cacheParameterGroupName = a} :: ModifyReplicationGroup)

-- | The number of days for which ElastiCache retains automatic node group
-- (shard) snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
-- backups are turned off.
modifyReplicationGroup_snapshotRetentionLimit :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Int)
modifyReplicationGroup_snapshotRetentionLimit = Lens.lens (\ModifyReplicationGroup' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ModifyReplicationGroup' {} a -> s {snapshotRetentionLimit = a} :: ModifyReplicationGroup)

-- | Deprecated. This parameter is not used.
modifyReplicationGroup_nodeGroupId :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_nodeGroupId = Lens.lens (\ModifyReplicationGroup' {nodeGroupId} -> nodeGroupId) (\s@ModifyReplicationGroup' {} a -> s {nodeGroupId = a} :: ModifyReplicationGroup)

-- | A list of user group IDs.
modifyReplicationGroup_userGroupIdsToAdd :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [Prelude.Text])
modifyReplicationGroup_userGroupIdsToAdd = Lens.lens (\ModifyReplicationGroup' {userGroupIdsToAdd} -> userGroupIdsToAdd) (\s@ModifyReplicationGroup' {} a -> s {userGroupIdsToAdd = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>.
modifyReplicationGroup_multiAZEnabled :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_multiAZEnabled = Lens.lens (\ModifyReplicationGroup' {multiAZEnabled} -> multiAZEnabled) (\s@ModifyReplicationGroup' {} a -> s {multiAZEnabled = a} :: ModifyReplicationGroup)

-- | The cluster ID that is used as the daily snapshot source for the
-- replication group. This parameter cannot be set for Redis (cluster mode
-- enabled) replication groups.
modifyReplicationGroup_snapshottingClusterId :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_snapshottingClusterId = Lens.lens (\ModifyReplicationGroup' {snapshottingClusterId} -> snapshottingClusterId) (\s@ModifyReplicationGroup' {} a -> s {snapshottingClusterId = a} :: ModifyReplicationGroup)

-- | The upgraded version of the cache engine to be run on the clusters in
-- the replication group.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
-- but you cannot downgrade to an earlier engine version. If you want to
-- use an earlier engine version, you must delete the existing replication
-- group and create it anew with the earlier engine version.
modifyReplicationGroup_engineVersion :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_engineVersion = Lens.lens (\ModifyReplicationGroup' {engineVersion} -> engineVersion) (\s@ModifyReplicationGroup' {} a -> s {engineVersion = a} :: ModifyReplicationGroup)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
modifyReplicationGroup_preferredMaintenanceWindow :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_preferredMaintenanceWindow = Lens.lens (\ModifyReplicationGroup' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyReplicationGroup' {} a -> s {preferredMaintenanceWindow = a} :: ModifyReplicationGroup)

-- | A valid cache node type that you want to scale this replication group
-- to.
modifyReplicationGroup_cacheNodeType :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_cacheNodeType = Lens.lens (\ModifyReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ModifyReplicationGroup' {} a -> s {cacheNodeType = a} :: ModifyReplicationGroup)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications are sent.
--
-- The Amazon SNS topic owner must be same as the replication group owner.
modifyReplicationGroup_notificationTopicArn :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_notificationTopicArn = Lens.lens (\ModifyReplicationGroup' {notificationTopicArn} -> notificationTopicArn) (\s@ModifyReplicationGroup' {} a -> s {notificationTopicArn = a} :: ModifyReplicationGroup)

-- | Reserved parameter. The password used to access a password protected
-- server. This parameter must be specified with the
-- @auth-token-update-strategy @ parameter. Password constraints:
--
-- -   Must be only printable ASCII characters
--
-- -   Must be at least 16 characters and no more than 128 characters in
--     length
--
-- -   Cannot contain any of the following characters: \'\/\', \'\"\', or
--     \'\@\', \'%\'
--
-- For more information, see AUTH password at
-- <http://redis.io/commands/AUTH AUTH>.
modifyReplicationGroup_authToken :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_authToken = Lens.lens (\ModifyReplicationGroup' {authToken} -> authToken) (\s@ModifyReplicationGroup' {} a -> s {authToken = a} :: ModifyReplicationGroup)

-- | Removes the user groups that can access this replication group.
modifyReplicationGroup_removeUserGroups :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_removeUserGroups = Lens.lens (\ModifyReplicationGroup' {removeUserGroups} -> removeUserGroups) (\s@ModifyReplicationGroup' {} a -> s {removeUserGroups = a} :: ModifyReplicationGroup)

-- | If @true@, this parameter causes the modifications in this request and
-- any pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the @PreferredMaintenanceWindow@ setting for the
-- replication group.
--
-- If @false@, changes to the nodes in the replication group are applied on
-- the next maintenance reboot, or the next failure reboot, whichever
-- occurs first.
--
-- Valid values: @true@ | @false@
--
-- Default: @false@
modifyReplicationGroup_applyImmediately :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_applyImmediately = Lens.lens (\ModifyReplicationGroup' {applyImmediately} -> applyImmediately) (\s@ModifyReplicationGroup' {} a -> s {applyImmediately = a} :: ModifyReplicationGroup)

-- | This parameter is currently disabled.
modifyReplicationGroup_autoMinorVersionUpgrade :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_autoMinorVersionUpgrade = Lens.lens (\ModifyReplicationGroup' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyReplicationGroup' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyReplicationGroup)

-- | The identifier of the replication group to modify.
modifyReplicationGroup_replicationGroupId :: Lens.Lens' ModifyReplicationGroup Prelude.Text
modifyReplicationGroup_replicationGroupId = Lens.lens (\ModifyReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@ModifyReplicationGroup' {} a -> s {replicationGroupId = a} :: ModifyReplicationGroup)

instance Prelude.AWSRequest ModifyReplicationGroup where
  type
    Rs ModifyReplicationGroup =
      ModifyReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyReplicationGroupResult"
      ( \s h x ->
          ModifyReplicationGroupResponse'
            Prelude.<$> (x Prelude..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyReplicationGroup

instance Prelude.NFData ModifyReplicationGroup

instance Prelude.ToHeaders ModifyReplicationGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyReplicationGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyReplicationGroup where
  toQuery ModifyReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyReplicationGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "SecurityGroupIds"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "SecurityGroupId"
                Prelude.<$> securityGroupIds
            ),
        "AutomaticFailoverEnabled"
          Prelude.=: automaticFailoverEnabled,
        "AuthTokenUpdateStrategy"
          Prelude.=: authTokenUpdateStrategy,
        "CacheSecurityGroupNames"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "CacheSecurityGroupName"
                Prelude.<$> cacheSecurityGroupNames
            ),
        "PrimaryClusterId" Prelude.=: primaryClusterId,
        "SnapshotWindow" Prelude.=: snapshotWindow,
        "NotificationTopicStatus"
          Prelude.=: notificationTopicStatus,
        "UserGroupIdsToRemove"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> userGroupIdsToRemove
            ),
        "ReplicationGroupDescription"
          Prelude.=: replicationGroupDescription,
        "CacheParameterGroupName"
          Prelude.=: cacheParameterGroupName,
        "SnapshotRetentionLimit"
          Prelude.=: snapshotRetentionLimit,
        "NodeGroupId" Prelude.=: nodeGroupId,
        "UserGroupIdsToAdd"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> userGroupIdsToAdd
            ),
        "MultiAZEnabled" Prelude.=: multiAZEnabled,
        "SnapshottingClusterId"
          Prelude.=: snapshottingClusterId,
        "EngineVersion" Prelude.=: engineVersion,
        "PreferredMaintenanceWindow"
          Prelude.=: preferredMaintenanceWindow,
        "CacheNodeType" Prelude.=: cacheNodeType,
        "NotificationTopicArn"
          Prelude.=: notificationTopicArn,
        "AuthToken" Prelude.=: authToken,
        "RemoveUserGroups" Prelude.=: removeUserGroups,
        "ApplyImmediately" Prelude.=: applyImmediately,
        "AutoMinorVersionUpgrade"
          Prelude.=: autoMinorVersionUpgrade,
        "ReplicationGroupId" Prelude.=: replicationGroupId
      ]

-- | /See:/ 'newModifyReplicationGroupResponse' smart constructor.
data ModifyReplicationGroupResponse = ModifyReplicationGroupResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'modifyReplicationGroupResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'modifyReplicationGroupResponse_httpStatus' - The response's http status code.
newModifyReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyReplicationGroupResponse
newModifyReplicationGroupResponse pHttpStatus_ =
  ModifyReplicationGroupResponse'
    { replicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyReplicationGroupResponse_replicationGroup :: Lens.Lens' ModifyReplicationGroupResponse (Prelude.Maybe ReplicationGroup)
modifyReplicationGroupResponse_replicationGroup = Lens.lens (\ModifyReplicationGroupResponse' {replicationGroup} -> replicationGroup) (\s@ModifyReplicationGroupResponse' {} a -> s {replicationGroup = a} :: ModifyReplicationGroupResponse)

-- | The response's http status code.
modifyReplicationGroupResponse_httpStatus :: Lens.Lens' ModifyReplicationGroupResponse Prelude.Int
modifyReplicationGroupResponse_httpStatus = Lens.lens (\ModifyReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@ModifyReplicationGroupResponse' {} a -> s {httpStatus = a} :: ModifyReplicationGroupResponse)

instance
  Prelude.NFData
    ModifyReplicationGroupResponse
