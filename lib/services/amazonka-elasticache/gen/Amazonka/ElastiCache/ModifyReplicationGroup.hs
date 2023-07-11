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
-- Module      : Amazonka.ElastiCache.ModifyReplicationGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ElastiCache.ModifyReplicationGroup
  ( -- * Creating a Request
    ModifyReplicationGroup (..),
    newModifyReplicationGroup,

    -- * Request Lenses
    modifyReplicationGroup_applyImmediately,
    modifyReplicationGroup_authToken,
    modifyReplicationGroup_authTokenUpdateStrategy,
    modifyReplicationGroup_autoMinorVersionUpgrade,
    modifyReplicationGroup_automaticFailoverEnabled,
    modifyReplicationGroup_cacheNodeType,
    modifyReplicationGroup_cacheParameterGroupName,
    modifyReplicationGroup_cacheSecurityGroupNames,
    modifyReplicationGroup_engineVersion,
    modifyReplicationGroup_ipDiscovery,
    modifyReplicationGroup_logDeliveryConfigurations,
    modifyReplicationGroup_multiAZEnabled,
    modifyReplicationGroup_nodeGroupId,
    modifyReplicationGroup_notificationTopicArn,
    modifyReplicationGroup_notificationTopicStatus,
    modifyReplicationGroup_preferredMaintenanceWindow,
    modifyReplicationGroup_primaryClusterId,
    modifyReplicationGroup_removeUserGroups,
    modifyReplicationGroup_replicationGroupDescription,
    modifyReplicationGroup_securityGroupIds,
    modifyReplicationGroup_snapshotRetentionLimit,
    modifyReplicationGroup_snapshotWindow,
    modifyReplicationGroup_snapshottingClusterId,
    modifyReplicationGroup_transitEncryptionEnabled,
    modifyReplicationGroup_transitEncryptionMode,
    modifyReplicationGroup_userGroupIdsToAdd,
    modifyReplicationGroup_userGroupIdsToRemove,
    modifyReplicationGroup_replicationGroupId,

    -- * Destructuring the Response
    ModifyReplicationGroupResponse (..),
    newModifyReplicationGroupResponse,

    -- * Response Lenses
    modifyReplicationGroupResponse_replicationGroup,
    modifyReplicationGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ModifyReplicationGroups@ operation.
--
-- /See:/ 'newModifyReplicationGroup' smart constructor.
data ModifyReplicationGroup = ModifyReplicationGroup'
  { -- | If @true@, this parameter causes the modifications in this request and
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
    -- | If you are running Redis engine version 6.0 or later, set this
    -- parameter to yes if you want to opt-in to the next auto minor version
    -- upgrade campaign. This parameter is disabled for previous versions.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether a read replica is automatically promoted to
    -- read\/write primary if the existing primary encounters a failure.
    --
    -- Valid values: @true@ | @false@
    automaticFailoverEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A valid cache node type that you want to scale this replication group
    -- to.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache parameter group to apply to all of the clusters in
    -- this replication group. This change is asynchronously applied as soon as
    -- possible for parameters when the @ApplyImmediately@ parameter is
    -- specified as @true@ for this request.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
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
    -- | The upgraded version of the cache engine to be run on the clusters in
    -- the replication group.
    --
    -- __Important:__ You can upgrade to a newer engine version (see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
    -- but you cannot downgrade to an earlier engine version. If you want to
    -- use an earlier engine version, you must delete the existing replication
    -- group and create it anew with the earlier engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The network type you choose when modifying a cluster, either @ipv4@ |
    -- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
    -- onward or Memcached engine version 1.6.6 on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    ipDiscovery :: Prelude.Maybe IpDiscovery,
    -- | Specifies the destination, format and type of the logs.
    logDeliveryConfigurations :: Prelude.Maybe [LogDeliveryConfigurationRequest],
    -- | A flag to indicate MultiAZ is enabled.
    multiAZEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Deprecated. This parameter is not used.
    nodeGroupId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
    -- notifications are sent.
    --
    -- The Amazon SNS topic owner must be same as the replication group owner.
    notificationTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the Amazon SNS notification topic for the replication
    -- group. Notifications are sent only if the status is @active@.
    --
    -- Valid values: @active@ | @inactive@
    notificationTopicStatus :: Prelude.Maybe Prelude.Text,
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
    -- | For replication groups with a single primary, if this parameter is
    -- specified, ElastiCache promotes the specified cluster in the specified
    -- replication group to the primary role. The nodes of all other clusters
    -- in the replication group are read replicas.
    primaryClusterId :: Prelude.Maybe Prelude.Text,
    -- | Removes the user group associated with this replication group.
    removeUserGroups :: Prelude.Maybe Prelude.Bool,
    -- | A description for the replication group. Maximum length is 255
    -- characters.
    replicationGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | Specifies the VPC Security Groups associated with the clusters in the
    -- replication group.
    --
    -- This parameter can be used only with replication group containing
    -- clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The number of days for which ElastiCache retains automatic node group
    -- (shard) snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
    -- backups are turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of the node group (shard) specified by
    -- @SnapshottingClusterId@.
    --
    -- Example: @05:00-09:00@
    --
    -- If you do not specify this parameter, ElastiCache automatically chooses
    -- an appropriate time range.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The cluster ID that is used as the daily snapshot source for the
    -- replication group. This parameter cannot be set for Redis (cluster mode
    -- enabled) replication groups.
    snapshottingClusterId :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables in-transit encryption when set to true. If you are
    -- enabling in-transit encryption for an existing cluster, you must also
    -- set @TransitEncryptionMode@ to @preferred@.
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A setting that allows you to migrate your clients to use in-transit
    -- encryption, with no downtime.
    --
    -- You must set @TransitEncryptionEnabled@ to @true@, for your existing
    -- cluster, and set @TransitEncryptionMode@ to @preferred@ in the same
    -- request to allow both encrypted and unencrypted connections at the same
    -- time. Once you migrate all your Redis clients to use encrypted
    -- connections you can set the value to @required@ to allow encrypted
    -- connections only.
    --
    -- Setting @TransitEncryptionMode@ to @required@ is a two-step process that
    -- requires you to first set the @TransitEncryptionMode@ to @preferred@
    -- first, after that you can set @TransitEncryptionMode@ to @required@.
    transitEncryptionMode :: Prelude.Maybe TransitEncryptionMode,
    -- | The ID of the user group you are associating with the replication group.
    userGroupIdsToAdd :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the user group to disassociate from the replication group,
    -- meaning the users in the group no longer can access the replication
    -- group.
    userGroupIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the replication group to modify.
    replicationGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'autoMinorVersionUpgrade', 'modifyReplicationGroup_autoMinorVersionUpgrade' - If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
--
-- 'automaticFailoverEnabled', 'modifyReplicationGroup_automaticFailoverEnabled' - Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
--
-- 'cacheNodeType', 'modifyReplicationGroup_cacheNodeType' - A valid cache node type that you want to scale this replication group
-- to.
--
-- 'cacheParameterGroupName', 'modifyReplicationGroup_cacheParameterGroupName' - The name of the cache parameter group to apply to all of the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the @ApplyImmediately@ parameter is
-- specified as @true@ for this request.
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
-- 'engineVersion', 'modifyReplicationGroup_engineVersion' - The upgraded version of the cache engine to be run on the clusters in
-- the replication group.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
-- but you cannot downgrade to an earlier engine version. If you want to
-- use an earlier engine version, you must delete the existing replication
-- group and create it anew with the earlier engine version.
--
-- 'ipDiscovery', 'modifyReplicationGroup_ipDiscovery' - The network type you choose when modifying a cluster, either @ipv4@ |
-- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
-- onward or Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
--
-- 'logDeliveryConfigurations', 'modifyReplicationGroup_logDeliveryConfigurations' - Specifies the destination, format and type of the logs.
--
-- 'multiAZEnabled', 'modifyReplicationGroup_multiAZEnabled' - A flag to indicate MultiAZ is enabled.
--
-- 'nodeGroupId', 'modifyReplicationGroup_nodeGroupId' - Deprecated. This parameter is not used.
--
-- 'notificationTopicArn', 'modifyReplicationGroup_notificationTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications are sent.
--
-- The Amazon SNS topic owner must be same as the replication group owner.
--
-- 'notificationTopicStatus', 'modifyReplicationGroup_notificationTopicStatus' - The status of the Amazon SNS notification topic for the replication
-- group. Notifications are sent only if the status is @active@.
--
-- Valid values: @active@ | @inactive@
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
-- 'primaryClusterId', 'modifyReplicationGroup_primaryClusterId' - For replication groups with a single primary, if this parameter is
-- specified, ElastiCache promotes the specified cluster in the specified
-- replication group to the primary role. The nodes of all other clusters
-- in the replication group are read replicas.
--
-- 'removeUserGroups', 'modifyReplicationGroup_removeUserGroups' - Removes the user group associated with this replication group.
--
-- 'replicationGroupDescription', 'modifyReplicationGroup_replicationGroupDescription' - A description for the replication group. Maximum length is 255
-- characters.
--
-- 'securityGroupIds', 'modifyReplicationGroup_securityGroupIds' - Specifies the VPC Security Groups associated with the clusters in the
-- replication group.
--
-- This parameter can be used only with replication group containing
-- clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
--
-- 'snapshotRetentionLimit', 'modifyReplicationGroup_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic node group
-- (shard) snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
-- backups are turned off.
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
-- 'snapshottingClusterId', 'modifyReplicationGroup_snapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the
-- replication group. This parameter cannot be set for Redis (cluster mode
-- enabled) replication groups.
--
-- 'transitEncryptionEnabled', 'modifyReplicationGroup_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to true. If you are
-- enabling in-transit encryption for an existing cluster, you must also
-- set @TransitEncryptionMode@ to @preferred@.
--
-- 'transitEncryptionMode', 'modifyReplicationGroup_transitEncryptionMode' - A setting that allows you to migrate your clients to use in-transit
-- encryption, with no downtime.
--
-- You must set @TransitEncryptionEnabled@ to @true@, for your existing
-- cluster, and set @TransitEncryptionMode@ to @preferred@ in the same
-- request to allow both encrypted and unencrypted connections at the same
-- time. Once you migrate all your Redis clients to use encrypted
-- connections you can set the value to @required@ to allow encrypted
-- connections only.
--
-- Setting @TransitEncryptionMode@ to @required@ is a two-step process that
-- requires you to first set the @TransitEncryptionMode@ to @preferred@
-- first, after that you can set @TransitEncryptionMode@ to @required@.
--
-- 'userGroupIdsToAdd', 'modifyReplicationGroup_userGroupIdsToAdd' - The ID of the user group you are associating with the replication group.
--
-- 'userGroupIdsToRemove', 'modifyReplicationGroup_userGroupIdsToRemove' - The ID of the user group to disassociate from the replication group,
-- meaning the users in the group no longer can access the replication
-- group.
--
-- 'replicationGroupId', 'modifyReplicationGroup_replicationGroupId' - The identifier of the replication group to modify.
newModifyReplicationGroup ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  ModifyReplicationGroup
newModifyReplicationGroup pReplicationGroupId_ =
  ModifyReplicationGroup'
    { applyImmediately =
        Prelude.Nothing,
      authToken = Prelude.Nothing,
      authTokenUpdateStrategy = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      automaticFailoverEnabled = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      cacheParameterGroupName = Prelude.Nothing,
      cacheSecurityGroupNames = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      ipDiscovery = Prelude.Nothing,
      logDeliveryConfigurations = Prelude.Nothing,
      multiAZEnabled = Prelude.Nothing,
      nodeGroupId = Prelude.Nothing,
      notificationTopicArn = Prelude.Nothing,
      notificationTopicStatus = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      primaryClusterId = Prelude.Nothing,
      removeUserGroups = Prelude.Nothing,
      replicationGroupDescription = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      snapshottingClusterId = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing,
      transitEncryptionMode = Prelude.Nothing,
      userGroupIdsToAdd = Prelude.Nothing,
      userGroupIdsToRemove = Prelude.Nothing,
      replicationGroupId = pReplicationGroupId_
    }

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

-- | If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
modifyReplicationGroup_autoMinorVersionUpgrade :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_autoMinorVersionUpgrade = Lens.lens (\ModifyReplicationGroup' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyReplicationGroup' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyReplicationGroup)

-- | Determines whether a read replica is automatically promoted to
-- read\/write primary if the existing primary encounters a failure.
--
-- Valid values: @true@ | @false@
modifyReplicationGroup_automaticFailoverEnabled :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_automaticFailoverEnabled = Lens.lens (\ModifyReplicationGroup' {automaticFailoverEnabled} -> automaticFailoverEnabled) (\s@ModifyReplicationGroup' {} a -> s {automaticFailoverEnabled = a} :: ModifyReplicationGroup)

-- | A valid cache node type that you want to scale this replication group
-- to.
modifyReplicationGroup_cacheNodeType :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_cacheNodeType = Lens.lens (\ModifyReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@ModifyReplicationGroup' {} a -> s {cacheNodeType = a} :: ModifyReplicationGroup)

-- | The name of the cache parameter group to apply to all of the clusters in
-- this replication group. This change is asynchronously applied as soon as
-- possible for parameters when the @ApplyImmediately@ parameter is
-- specified as @true@ for this request.
modifyReplicationGroup_cacheParameterGroupName :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_cacheParameterGroupName = Lens.lens (\ModifyReplicationGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ModifyReplicationGroup' {} a -> s {cacheParameterGroupName = a} :: ModifyReplicationGroup)

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
modifyReplicationGroup_cacheSecurityGroupNames = Lens.lens (\ModifyReplicationGroup' {cacheSecurityGroupNames} -> cacheSecurityGroupNames) (\s@ModifyReplicationGroup' {} a -> s {cacheSecurityGroupNames = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Lens.coerced

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

-- | The network type you choose when modifying a cluster, either @ipv4@ |
-- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
-- onward or Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
modifyReplicationGroup_ipDiscovery :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe IpDiscovery)
modifyReplicationGroup_ipDiscovery = Lens.lens (\ModifyReplicationGroup' {ipDiscovery} -> ipDiscovery) (\s@ModifyReplicationGroup' {} a -> s {ipDiscovery = a} :: ModifyReplicationGroup)

-- | Specifies the destination, format and type of the logs.
modifyReplicationGroup_logDeliveryConfigurations :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [LogDeliveryConfigurationRequest])
modifyReplicationGroup_logDeliveryConfigurations = Lens.lens (\ModifyReplicationGroup' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@ModifyReplicationGroup' {} a -> s {logDeliveryConfigurations = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | A flag to indicate MultiAZ is enabled.
modifyReplicationGroup_multiAZEnabled :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_multiAZEnabled = Lens.lens (\ModifyReplicationGroup' {multiAZEnabled} -> multiAZEnabled) (\s@ModifyReplicationGroup' {} a -> s {multiAZEnabled = a} :: ModifyReplicationGroup)

-- | Deprecated. This parameter is not used.
modifyReplicationGroup_nodeGroupId :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_nodeGroupId = Lens.lens (\ModifyReplicationGroup' {nodeGroupId} -> nodeGroupId) (\s@ModifyReplicationGroup' {} a -> s {nodeGroupId = a} :: ModifyReplicationGroup)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications are sent.
--
-- The Amazon SNS topic owner must be same as the replication group owner.
modifyReplicationGroup_notificationTopicArn :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_notificationTopicArn = Lens.lens (\ModifyReplicationGroup' {notificationTopicArn} -> notificationTopicArn) (\s@ModifyReplicationGroup' {} a -> s {notificationTopicArn = a} :: ModifyReplicationGroup)

-- | The status of the Amazon SNS notification topic for the replication
-- group. Notifications are sent only if the status is @active@.
--
-- Valid values: @active@ | @inactive@
modifyReplicationGroup_notificationTopicStatus :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_notificationTopicStatus = Lens.lens (\ModifyReplicationGroup' {notificationTopicStatus} -> notificationTopicStatus) (\s@ModifyReplicationGroup' {} a -> s {notificationTopicStatus = a} :: ModifyReplicationGroup)

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

-- | For replication groups with a single primary, if this parameter is
-- specified, ElastiCache promotes the specified cluster in the specified
-- replication group to the primary role. The nodes of all other clusters
-- in the replication group are read replicas.
modifyReplicationGroup_primaryClusterId :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_primaryClusterId = Lens.lens (\ModifyReplicationGroup' {primaryClusterId} -> primaryClusterId) (\s@ModifyReplicationGroup' {} a -> s {primaryClusterId = a} :: ModifyReplicationGroup)

-- | Removes the user group associated with this replication group.
modifyReplicationGroup_removeUserGroups :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_removeUserGroups = Lens.lens (\ModifyReplicationGroup' {removeUserGroups} -> removeUserGroups) (\s@ModifyReplicationGroup' {} a -> s {removeUserGroups = a} :: ModifyReplicationGroup)

-- | A description for the replication group. Maximum length is 255
-- characters.
modifyReplicationGroup_replicationGroupDescription :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_replicationGroupDescription = Lens.lens (\ModifyReplicationGroup' {replicationGroupDescription} -> replicationGroupDescription) (\s@ModifyReplicationGroup' {} a -> s {replicationGroupDescription = a} :: ModifyReplicationGroup)

-- | Specifies the VPC Security Groups associated with the clusters in the
-- replication group.
--
-- This parameter can be used only with replication group containing
-- clusters running in an Amazon Virtual Private Cloud (Amazon VPC).
modifyReplicationGroup_securityGroupIds :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [Prelude.Text])
modifyReplicationGroup_securityGroupIds = Lens.lens (\ModifyReplicationGroup' {securityGroupIds} -> securityGroupIds) (\s@ModifyReplicationGroup' {} a -> s {securityGroupIds = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which ElastiCache retains automatic node group
-- (shard) snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- __Important__ If the value of SnapshotRetentionLimit is set to zero (0),
-- backups are turned off.
modifyReplicationGroup_snapshotRetentionLimit :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Int)
modifyReplicationGroup_snapshotRetentionLimit = Lens.lens (\ModifyReplicationGroup' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ModifyReplicationGroup' {} a -> s {snapshotRetentionLimit = a} :: ModifyReplicationGroup)

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

-- | The cluster ID that is used as the daily snapshot source for the
-- replication group. This parameter cannot be set for Redis (cluster mode
-- enabled) replication groups.
modifyReplicationGroup_snapshottingClusterId :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Text)
modifyReplicationGroup_snapshottingClusterId = Lens.lens (\ModifyReplicationGroup' {snapshottingClusterId} -> snapshottingClusterId) (\s@ModifyReplicationGroup' {} a -> s {snapshottingClusterId = a} :: ModifyReplicationGroup)

-- | A flag that enables in-transit encryption when set to true. If you are
-- enabling in-transit encryption for an existing cluster, you must also
-- set @TransitEncryptionMode@ to @preferred@.
modifyReplicationGroup_transitEncryptionEnabled :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe Prelude.Bool)
modifyReplicationGroup_transitEncryptionEnabled = Lens.lens (\ModifyReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@ModifyReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: ModifyReplicationGroup)

-- | A setting that allows you to migrate your clients to use in-transit
-- encryption, with no downtime.
--
-- You must set @TransitEncryptionEnabled@ to @true@, for your existing
-- cluster, and set @TransitEncryptionMode@ to @preferred@ in the same
-- request to allow both encrypted and unencrypted connections at the same
-- time. Once you migrate all your Redis clients to use encrypted
-- connections you can set the value to @required@ to allow encrypted
-- connections only.
--
-- Setting @TransitEncryptionMode@ to @required@ is a two-step process that
-- requires you to first set the @TransitEncryptionMode@ to @preferred@
-- first, after that you can set @TransitEncryptionMode@ to @required@.
modifyReplicationGroup_transitEncryptionMode :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe TransitEncryptionMode)
modifyReplicationGroup_transitEncryptionMode = Lens.lens (\ModifyReplicationGroup' {transitEncryptionMode} -> transitEncryptionMode) (\s@ModifyReplicationGroup' {} a -> s {transitEncryptionMode = a} :: ModifyReplicationGroup)

-- | The ID of the user group you are associating with the replication group.
modifyReplicationGroup_userGroupIdsToAdd :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [Prelude.Text])
modifyReplicationGroup_userGroupIdsToAdd = Lens.lens (\ModifyReplicationGroup' {userGroupIdsToAdd} -> userGroupIdsToAdd) (\s@ModifyReplicationGroup' {} a -> s {userGroupIdsToAdd = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the user group to disassociate from the replication group,
-- meaning the users in the group no longer can access the replication
-- group.
modifyReplicationGroup_userGroupIdsToRemove :: Lens.Lens' ModifyReplicationGroup (Prelude.Maybe [Prelude.Text])
modifyReplicationGroup_userGroupIdsToRemove = Lens.lens (\ModifyReplicationGroup' {userGroupIdsToRemove} -> userGroupIdsToRemove) (\s@ModifyReplicationGroup' {} a -> s {userGroupIdsToRemove = a} :: ModifyReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the replication group to modify.
modifyReplicationGroup_replicationGroupId :: Lens.Lens' ModifyReplicationGroup Prelude.Text
modifyReplicationGroup_replicationGroupId = Lens.lens (\ModifyReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@ModifyReplicationGroup' {} a -> s {replicationGroupId = a} :: ModifyReplicationGroup)

instance Core.AWSRequest ModifyReplicationGroup where
  type
    AWSResponse ModifyReplicationGroup =
      ModifyReplicationGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyReplicationGroupResult"
      ( \s h x ->
          ModifyReplicationGroupResponse'
            Prelude.<$> (x Data..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyReplicationGroup where
  hashWithSalt _salt ModifyReplicationGroup' {..} =
    _salt
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` authToken
      `Prelude.hashWithSalt` authTokenUpdateStrategy
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` automaticFailoverEnabled
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` cacheParameterGroupName
      `Prelude.hashWithSalt` cacheSecurityGroupNames
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` ipDiscovery
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` multiAZEnabled
      `Prelude.hashWithSalt` nodeGroupId
      `Prelude.hashWithSalt` notificationTopicArn
      `Prelude.hashWithSalt` notificationTopicStatus
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` primaryClusterId
      `Prelude.hashWithSalt` removeUserGroups
      `Prelude.hashWithSalt` replicationGroupDescription
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` snapshottingClusterId
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` transitEncryptionMode
      `Prelude.hashWithSalt` userGroupIdsToAdd
      `Prelude.hashWithSalt` userGroupIdsToRemove
      `Prelude.hashWithSalt` replicationGroupId

instance Prelude.NFData ModifyReplicationGroup where
  rnf ModifyReplicationGroup' {..} =
    Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf authToken
      `Prelude.seq` Prelude.rnf authTokenUpdateStrategy
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf automaticFailoverEnabled
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf cacheParameterGroupName
      `Prelude.seq` Prelude.rnf cacheSecurityGroupNames
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf ipDiscovery
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf multiAZEnabled
      `Prelude.seq` Prelude.rnf nodeGroupId
      `Prelude.seq` Prelude.rnf notificationTopicArn
      `Prelude.seq` Prelude.rnf notificationTopicStatus
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf primaryClusterId
      `Prelude.seq` Prelude.rnf removeUserGroups
      `Prelude.seq` Prelude.rnf
        replicationGroupDescription
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf
        snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf
        snapshottingClusterId
      `Prelude.seq` Prelude.rnf
        transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf
        transitEncryptionMode
      `Prelude.seq` Prelude.rnf
        userGroupIdsToAdd
      `Prelude.seq` Prelude.rnf
        userGroupIdsToRemove
      `Prelude.seq` Prelude.rnf
        replicationGroupId

instance Data.ToHeaders ModifyReplicationGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyReplicationGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyReplicationGroup where
  toQuery ModifyReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyReplicationGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "ApplyImmediately" Data.=: applyImmediately,
        "AuthToken" Data.=: authToken,
        "AuthTokenUpdateStrategy"
          Data.=: authTokenUpdateStrategy,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "AutomaticFailoverEnabled"
          Data.=: automaticFailoverEnabled,
        "CacheNodeType" Data.=: cacheNodeType,
        "CacheParameterGroupName"
          Data.=: cacheParameterGroupName,
        "CacheSecurityGroupNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "CacheSecurityGroupName"
                Prelude.<$> cacheSecurityGroupNames
            ),
        "EngineVersion" Data.=: engineVersion,
        "IpDiscovery" Data.=: ipDiscovery,
        "LogDeliveryConfigurations"
          Data.=: Data.toQuery
            ( Data.toQueryList "LogDeliveryConfigurationRequest"
                Prelude.<$> logDeliveryConfigurations
            ),
        "MultiAZEnabled" Data.=: multiAZEnabled,
        "NodeGroupId" Data.=: nodeGroupId,
        "NotificationTopicArn" Data.=: notificationTopicArn,
        "NotificationTopicStatus"
          Data.=: notificationTopicStatus,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "PrimaryClusterId" Data.=: primaryClusterId,
        "RemoveUserGroups" Data.=: removeUserGroups,
        "ReplicationGroupDescription"
          Data.=: replicationGroupDescription,
        "SecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "SecurityGroupId"
                Prelude.<$> securityGroupIds
            ),
        "SnapshotRetentionLimit"
          Data.=: snapshotRetentionLimit,
        "SnapshotWindow" Data.=: snapshotWindow,
        "SnapshottingClusterId"
          Data.=: snapshottingClusterId,
        "TransitEncryptionEnabled"
          Data.=: transitEncryptionEnabled,
        "TransitEncryptionMode"
          Data.=: transitEncryptionMode,
        "UserGroupIdsToAdd"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> userGroupIdsToAdd
            ),
        "UserGroupIdsToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> userGroupIdsToRemove
            ),
        "ReplicationGroupId" Data.=: replicationGroupId
      ]

-- | /See:/ 'newModifyReplicationGroupResponse' smart constructor.
data ModifyReplicationGroupResponse = ModifyReplicationGroupResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf ModifyReplicationGroupResponse' {..} =
    Prelude.rnf replicationGroup
      `Prelude.seq` Prelude.rnf httpStatus
