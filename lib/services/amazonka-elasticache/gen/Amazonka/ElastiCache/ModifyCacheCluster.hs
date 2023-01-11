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
-- Module      : Amazonka.ElastiCache.ModifyCacheCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a cluster. You can use this operation to
-- change one or more cluster configuration parameters by specifying the
-- parameters and the new values.
module Amazonka.ElastiCache.ModifyCacheCluster
  ( -- * Creating a Request
    ModifyCacheCluster (..),
    newModifyCacheCluster,

    -- * Request Lenses
    modifyCacheCluster_aZMode,
    modifyCacheCluster_applyImmediately,
    modifyCacheCluster_authToken,
    modifyCacheCluster_authTokenUpdateStrategy,
    modifyCacheCluster_autoMinorVersionUpgrade,
    modifyCacheCluster_cacheNodeIdsToRemove,
    modifyCacheCluster_cacheNodeType,
    modifyCacheCluster_cacheParameterGroupName,
    modifyCacheCluster_cacheSecurityGroupNames,
    modifyCacheCluster_engineVersion,
    modifyCacheCluster_ipDiscovery,
    modifyCacheCluster_logDeliveryConfigurations,
    modifyCacheCluster_newAvailabilityZones,
    modifyCacheCluster_notificationTopicArn,
    modifyCacheCluster_notificationTopicStatus,
    modifyCacheCluster_numCacheNodes,
    modifyCacheCluster_preferredMaintenanceWindow,
    modifyCacheCluster_securityGroupIds,
    modifyCacheCluster_snapshotRetentionLimit,
    modifyCacheCluster_snapshotWindow,
    modifyCacheCluster_cacheClusterId,

    -- * Destructuring the Response
    ModifyCacheClusterResponse (..),
    newModifyCacheClusterResponse,

    -- * Response Lenses
    modifyCacheClusterResponse_cacheCluster,
    modifyCacheClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @ModifyCacheCluster@ operation.
--
-- /See:/ 'newModifyCacheCluster' smart constructor.
data ModifyCacheCluster = ModifyCacheCluster'
  { -- | Specifies whether the new nodes in this Memcached cluster are all
    -- created in a single Availability Zone or created across multiple
    -- Availability Zones.
    --
    -- Valid values: @single-az@ | @cross-az@.
    --
    -- This option is only supported for Memcached clusters.
    --
    -- You cannot specify @single-az@ if the Memcached cluster already has
    -- cache nodes in different Availability Zones. If @cross-az@ is specified,
    -- existing Memcached nodes remain in their current Availability Zone.
    --
    -- Only newly created nodes are located in different Availability Zones.
    aZMode :: Prelude.Maybe AZMode,
    -- | If @true@, this parameter causes the modifications in this request and
    -- any pending modifications to be applied, asynchronously and as soon as
    -- possible, regardless of the @PreferredMaintenanceWindow@ setting for the
    -- cluster.
    --
    -- If @false@, changes to the cluster are applied on the next maintenance
    -- reboot, or the next failure reboot, whichever occurs first.
    --
    -- If you perform a @ModifyCacheCluster@ before a pending modification is
    -- applied, the pending modification is replaced by the newer modification.
    --
    -- Valid values: @true@ | @false@
    --
    -- Default: @false@
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | Reserved parameter. The password used to access a password protected
    -- server. This parameter must be specified with the @auth-token-update@
    -- parameter. Password constraints:
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
    -- | A list of cache node IDs to be removed. A node ID is a numeric
    -- identifier (0001, 0002, etc.). This parameter is only valid when
    -- @NumCacheNodes@ is less than the existing number of cache nodes. The
    -- number of cache node IDs supplied in this parameter must match the
    -- difference between the existing number of cache nodes in the cluster or
    -- pending cache nodes, whichever is greater, and the value of
    -- @NumCacheNodes@ in the request.
    --
    -- For example: If you have 3 active cache nodes, 7 pending cache nodes,
    -- and the number of cache nodes in this @ModifyCacheCluster@ call is 5,
    -- you must list 2 (7 - 5) cache node IDs to remove.
    cacheNodeIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | A valid cache node type that you want to scale this cluster up to.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache parameter group to apply to this cluster. This
    -- change is asynchronously applied as soon as possible for parameters when
    -- the @ApplyImmediately@ parameter is specified as @true@ for this
    -- request.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of cache security group names to authorize on this cluster. This
    -- change is asynchronously applied as soon as possible.
    --
    -- You can use this parameter only with clusters that are created outside
    -- of an Amazon Virtual Private Cloud (Amazon VPC).
    --
    -- Constraints: Must contain no more than 255 alphanumeric characters. Must
    -- not be \"Default\".
    cacheSecurityGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The upgraded version of the cache engine to be run on the cache nodes.
    --
    -- __Important:__ You can upgrade to a newer engine version (see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
    -- but you cannot downgrade to an earlier engine version. If you want to
    -- use an earlier engine version, you must delete the existing cluster and
    -- create it anew with the earlier engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The network type you choose when modifying a cluster, either @ipv4@ |
    -- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
    -- onward or Memcached engine version 1.6.6 on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    ipDiscovery :: Prelude.Maybe IpDiscovery,
    -- | Specifies the destination, format and type of the logs.
    logDeliveryConfigurations :: Prelude.Maybe [LogDeliveryConfigurationRequest],
    -- | This option is only supported on Memcached clusters.
    --
    -- The list of Availability Zones where the new Memcached cache nodes are
    -- created.
    --
    -- This parameter is only valid when @NumCacheNodes@ in the request is
    -- greater than the sum of the number of active cache nodes and the number
    -- of cache nodes pending creation (which may be zero). The number of
    -- Availability Zones supplied in this list must match the cache nodes
    -- being added in this request.
    --
    -- Scenarios:
    --
    -- -   __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes.
    --     Specify @NumCacheNodes=5@ (3 + 2) and optionally specify two
    --     Availability Zones for the two new nodes.
    --
    -- -   __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation
    --     (from the scenario 1 call) and want to add 1 more node. Specify
    --     @NumCacheNodes=6@ ((3 + 2) + 1) and optionally specify an
    --     Availability Zone for the new node.
    --
    -- -   __Scenario 3:__ You want to cancel all pending operations. Specify
    --     @NumCacheNodes=3@ to cancel all pending operations.
    --
    -- The Availability Zone placement of nodes pending creation cannot be
    -- modified. If you wish to cancel any nodes pending creation, add 0 nodes
    -- by setting @NumCacheNodes@ to the number of current nodes.
    --
    -- If @cross-az@ is specified, existing Memcached nodes remain in their
    -- current Availability Zone. Only newly created nodes can be located in
    -- different Availability Zones. For guidance on how to move existing
    -- Memcached nodes to different Availability Zones, see the __Availability
    -- Zone Considerations__ section of
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/CacheNodes.SupportedTypes.html Cache Node Considerations for Memcached>.
    --
    -- __Impact of new add\/remove requests upon pending requests__
    --
    -- -   Scenario-1
    --
    --     -   Pending Action: Delete
    --
    --     -   New Request: Delete
    --
    --     -   Result: The new delete, pending or immediate, replaces the
    --         pending delete.
    --
    -- -   Scenario-2
    --
    --     -   Pending Action: Delete
    --
    --     -   New Request: Create
    --
    --     -   Result: The new create, pending or immediate, replaces the
    --         pending delete.
    --
    -- -   Scenario-3
    --
    --     -   Pending Action: Create
    --
    --     -   New Request: Delete
    --
    --     -   Result: The new delete, pending or immediate, replaces the
    --         pending create.
    --
    -- -   Scenario-4
    --
    --     -   Pending Action: Create
    --
    --     -   New Request: Create
    --
    --     -   Result: The new create is added to the pending create.
    --
    --         __Important:__ If the new create request is __Apply Immediately
    --         - Yes__, all creates are performed immediately. If the new
    --         create request is __Apply Immediately - No__, all creates are
    --         pending.
    newAvailabilityZones' :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
    -- notifications are sent.
    --
    -- The Amazon SNS topic owner must be same as the cluster owner.
    notificationTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the Amazon SNS notification topic. Notifications are sent
    -- only if the status is @active@.
    --
    -- Valid values: @active@ | @inactive@
    notificationTopicStatus :: Prelude.Maybe Prelude.Text,
    -- | The number of cache nodes that the cluster should have. If the value for
    -- @NumCacheNodes@ is greater than the sum of the number of current cache
    -- nodes and the number of cache nodes pending creation (which may be
    -- zero), more nodes are added. If the value is less than the number of
    -- existing cache nodes, nodes are removed. If the value is equal to the
    -- number of current cache nodes, any pending add or remove requests are
    -- canceled.
    --
    -- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@
    -- parameter to provide the IDs of the specific cache nodes to remove.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 40.
    --
    -- Adding or removing Memcached cache nodes can be applied immediately or
    -- as a pending operation (see @ApplyImmediately@).
    --
    -- A pending operation to modify the number of cache nodes in a cluster
    -- during its maintenance window, whether by adding or removing nodes in
    -- accordance with the scale out architecture, is not queued. The
    -- customer\'s latest request to add or remove nodes to the cluster
    -- overrides any previous pending operations to modify the number of cache
    -- nodes in the cluster. For example, a request to remove 2 nodes would
    -- override a previous pending operation to remove 3 nodes. Similarly, a
    -- request to add 2 nodes would override a previous pending operation to
    -- remove 3 nodes and vice versa. As Memcached cache nodes may now be
    -- provisioned in different Availability Zones with flexible cache node
    -- placement, a request to add nodes does not automatically override a
    -- previous pending operation to add nodes. The customer can modify the
    -- previous pending operation to add more nodes or explicitly cancel the
    -- pending request and retry the new request. To cancel pending operations
    -- to modify the number of cache nodes in a cluster, use the
    -- @ModifyCacheCluster@ request and set @NumCacheNodes@ equal to the number
    -- of cache nodes currently in the cluster.
    numCacheNodes :: Prelude.Maybe Prelude.Int,
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
    -- | Specifies the VPC Security Groups associated with the cluster.
    --
    -- This parameter can be used only with clusters that are created in an
    -- Amazon Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The number of days for which ElastiCache retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
    -- turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of your cluster.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier. This value is stored as a lowercase string.
    cacheClusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCacheCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aZMode', 'modifyCacheCluster_aZMode' - Specifies whether the new nodes in this Memcached cluster are all
-- created in a single Availability Zone or created across multiple
-- Availability Zones.
--
-- Valid values: @single-az@ | @cross-az@.
--
-- This option is only supported for Memcached clusters.
--
-- You cannot specify @single-az@ if the Memcached cluster already has
-- cache nodes in different Availability Zones. If @cross-az@ is specified,
-- existing Memcached nodes remain in their current Availability Zone.
--
-- Only newly created nodes are located in different Availability Zones.
--
-- 'applyImmediately', 'modifyCacheCluster_applyImmediately' - If @true@, this parameter causes the modifications in this request and
-- any pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the @PreferredMaintenanceWindow@ setting for the
-- cluster.
--
-- If @false@, changes to the cluster are applied on the next maintenance
-- reboot, or the next failure reboot, whichever occurs first.
--
-- If you perform a @ModifyCacheCluster@ before a pending modification is
-- applied, the pending modification is replaced by the newer modification.
--
-- Valid values: @true@ | @false@
--
-- Default: @false@
--
-- 'authToken', 'modifyCacheCluster_authToken' - Reserved parameter. The password used to access a password protected
-- server. This parameter must be specified with the @auth-token-update@
-- parameter. Password constraints:
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
-- 'authTokenUpdateStrategy', 'modifyCacheCluster_authTokenUpdateStrategy' - Specifies the strategy to use to update the AUTH token. This parameter
-- must be specified with the @auth-token@ parameter. Possible values:
--
-- -   Rotate
--
-- -   Set
--
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
--
-- 'autoMinorVersionUpgrade', 'modifyCacheCluster_autoMinorVersionUpgrade' - If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
--
-- 'cacheNodeIdsToRemove', 'modifyCacheCluster_cacheNodeIdsToRemove' - A list of cache node IDs to be removed. A node ID is a numeric
-- identifier (0001, 0002, etc.). This parameter is only valid when
-- @NumCacheNodes@ is less than the existing number of cache nodes. The
-- number of cache node IDs supplied in this parameter must match the
-- difference between the existing number of cache nodes in the cluster or
-- pending cache nodes, whichever is greater, and the value of
-- @NumCacheNodes@ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes,
-- and the number of cache nodes in this @ModifyCacheCluster@ call is 5,
-- you must list 2 (7 - 5) cache node IDs to remove.
--
-- 'cacheNodeType', 'modifyCacheCluster_cacheNodeType' - A valid cache node type that you want to scale this cluster up to.
--
-- 'cacheParameterGroupName', 'modifyCacheCluster_cacheParameterGroupName' - The name of the cache parameter group to apply to this cluster. This
-- change is asynchronously applied as soon as possible for parameters when
-- the @ApplyImmediately@ parameter is specified as @true@ for this
-- request.
--
-- 'cacheSecurityGroupNames', 'modifyCacheCluster_cacheSecurityGroupNames' - A list of cache security group names to authorize on this cluster. This
-- change is asynchronously applied as soon as possible.
--
-- You can use this parameter only with clusters that are created outside
-- of an Amazon Virtual Private Cloud (Amazon VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must
-- not be \"Default\".
--
-- 'engineVersion', 'modifyCacheCluster_engineVersion' - The upgraded version of the cache engine to be run on the cache nodes.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
-- but you cannot downgrade to an earlier engine version. If you want to
-- use an earlier engine version, you must delete the existing cluster and
-- create it anew with the earlier engine version.
--
-- 'ipDiscovery', 'modifyCacheCluster_ipDiscovery' - The network type you choose when modifying a cluster, either @ipv4@ |
-- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
-- onward or Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
--
-- 'logDeliveryConfigurations', 'modifyCacheCluster_logDeliveryConfigurations' - Specifies the destination, format and type of the logs.
--
-- 'newAvailabilityZones'', 'modifyCacheCluster_newAvailabilityZones' - This option is only supported on Memcached clusters.
--
-- The list of Availability Zones where the new Memcached cache nodes are
-- created.
--
-- This parameter is only valid when @NumCacheNodes@ in the request is
-- greater than the sum of the number of active cache nodes and the number
-- of cache nodes pending creation (which may be zero). The number of
-- Availability Zones supplied in this list must match the cache nodes
-- being added in this request.
--
-- Scenarios:
--
-- -   __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes.
--     Specify @NumCacheNodes=5@ (3 + 2) and optionally specify two
--     Availability Zones for the two new nodes.
--
-- -   __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation
--     (from the scenario 1 call) and want to add 1 more node. Specify
--     @NumCacheNodes=6@ ((3 + 2) + 1) and optionally specify an
--     Availability Zone for the new node.
--
-- -   __Scenario 3:__ You want to cancel all pending operations. Specify
--     @NumCacheNodes=3@ to cancel all pending operations.
--
-- The Availability Zone placement of nodes pending creation cannot be
-- modified. If you wish to cancel any nodes pending creation, add 0 nodes
-- by setting @NumCacheNodes@ to the number of current nodes.
--
-- If @cross-az@ is specified, existing Memcached nodes remain in their
-- current Availability Zone. Only newly created nodes can be located in
-- different Availability Zones. For guidance on how to move existing
-- Memcached nodes to different Availability Zones, see the __Availability
-- Zone Considerations__ section of
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/CacheNodes.SupportedTypes.html Cache Node Considerations for Memcached>.
--
-- __Impact of new add\/remove requests upon pending requests__
--
-- -   Scenario-1
--
--     -   Pending Action: Delete
--
--     -   New Request: Delete
--
--     -   Result: The new delete, pending or immediate, replaces the
--         pending delete.
--
-- -   Scenario-2
--
--     -   Pending Action: Delete
--
--     -   New Request: Create
--
--     -   Result: The new create, pending or immediate, replaces the
--         pending delete.
--
-- -   Scenario-3
--
--     -   Pending Action: Create
--
--     -   New Request: Delete
--
--     -   Result: The new delete, pending or immediate, replaces the
--         pending create.
--
-- -   Scenario-4
--
--     -   Pending Action: Create
--
--     -   New Request: Create
--
--     -   Result: The new create is added to the pending create.
--
--         __Important:__ If the new create request is __Apply Immediately
--         - Yes__, all creates are performed immediately. If the new
--         create request is __Apply Immediately - No__, all creates are
--         pending.
--
-- 'notificationTopicArn', 'modifyCacheCluster_notificationTopicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications are sent.
--
-- The Amazon SNS topic owner must be same as the cluster owner.
--
-- 'notificationTopicStatus', 'modifyCacheCluster_notificationTopicStatus' - The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is @active@.
--
-- Valid values: @active@ | @inactive@
--
-- 'numCacheNodes', 'modifyCacheCluster_numCacheNodes' - The number of cache nodes that the cluster should have. If the value for
-- @NumCacheNodes@ is greater than the sum of the number of current cache
-- nodes and the number of cache nodes pending creation (which may be
-- zero), more nodes are added. If the value is less than the number of
-- existing cache nodes, nodes are removed. If the value is equal to the
-- number of current cache nodes, any pending add or remove requests are
-- canceled.
--
-- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@
-- parameter to provide the IDs of the specific cache nodes to remove.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
--
-- Adding or removing Memcached cache nodes can be applied immediately or
-- as a pending operation (see @ApplyImmediately@).
--
-- A pending operation to modify the number of cache nodes in a cluster
-- during its maintenance window, whether by adding or removing nodes in
-- accordance with the scale out architecture, is not queued. The
-- customer\'s latest request to add or remove nodes to the cluster
-- overrides any previous pending operations to modify the number of cache
-- nodes in the cluster. For example, a request to remove 2 nodes would
-- override a previous pending operation to remove 3 nodes. Similarly, a
-- request to add 2 nodes would override a previous pending operation to
-- remove 3 nodes and vice versa. As Memcached cache nodes may now be
-- provisioned in different Availability Zones with flexible cache node
-- placement, a request to add nodes does not automatically override a
-- previous pending operation to add nodes. The customer can modify the
-- previous pending operation to add more nodes or explicitly cancel the
-- pending request and retry the new request. To cancel pending operations
-- to modify the number of cache nodes in a cluster, use the
-- @ModifyCacheCluster@ request and set @NumCacheNodes@ equal to the number
-- of cache nodes currently in the cluster.
--
-- 'preferredMaintenanceWindow', 'modifyCacheCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
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
-- 'securityGroupIds', 'modifyCacheCluster_securityGroupIds' - Specifies the VPC Security Groups associated with the cluster.
--
-- This parameter can be used only with clusters that are created in an
-- Amazon Virtual Private Cloud (Amazon VPC).
--
-- 'snapshotRetentionLimit', 'modifyCacheCluster_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
--
-- 'snapshotWindow', 'modifyCacheCluster_snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your cluster.
--
-- 'cacheClusterId', 'modifyCacheCluster_cacheClusterId' - The cluster identifier. This value is stored as a lowercase string.
newModifyCacheCluster ::
  -- | 'cacheClusterId'
  Prelude.Text ->
  ModifyCacheCluster
newModifyCacheCluster pCacheClusterId_ =
  ModifyCacheCluster'
    { aZMode = Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      authToken = Prelude.Nothing,
      authTokenUpdateStrategy = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      cacheNodeIdsToRemove = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      cacheParameterGroupName = Prelude.Nothing,
      cacheSecurityGroupNames = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      ipDiscovery = Prelude.Nothing,
      logDeliveryConfigurations = Prelude.Nothing,
      newAvailabilityZones' = Prelude.Nothing,
      notificationTopicArn = Prelude.Nothing,
      notificationTopicStatus = Prelude.Nothing,
      numCacheNodes = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      cacheClusterId = pCacheClusterId_
    }

-- | Specifies whether the new nodes in this Memcached cluster are all
-- created in a single Availability Zone or created across multiple
-- Availability Zones.
--
-- Valid values: @single-az@ | @cross-az@.
--
-- This option is only supported for Memcached clusters.
--
-- You cannot specify @single-az@ if the Memcached cluster already has
-- cache nodes in different Availability Zones. If @cross-az@ is specified,
-- existing Memcached nodes remain in their current Availability Zone.
--
-- Only newly created nodes are located in different Availability Zones.
modifyCacheCluster_aZMode :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe AZMode)
modifyCacheCluster_aZMode = Lens.lens (\ModifyCacheCluster' {aZMode} -> aZMode) (\s@ModifyCacheCluster' {} a -> s {aZMode = a} :: ModifyCacheCluster)

-- | If @true@, this parameter causes the modifications in this request and
-- any pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the @PreferredMaintenanceWindow@ setting for the
-- cluster.
--
-- If @false@, changes to the cluster are applied on the next maintenance
-- reboot, or the next failure reboot, whichever occurs first.
--
-- If you perform a @ModifyCacheCluster@ before a pending modification is
-- applied, the pending modification is replaced by the newer modification.
--
-- Valid values: @true@ | @false@
--
-- Default: @false@
modifyCacheCluster_applyImmediately :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Bool)
modifyCacheCluster_applyImmediately = Lens.lens (\ModifyCacheCluster' {applyImmediately} -> applyImmediately) (\s@ModifyCacheCluster' {} a -> s {applyImmediately = a} :: ModifyCacheCluster)

-- | Reserved parameter. The password used to access a password protected
-- server. This parameter must be specified with the @auth-token-update@
-- parameter. Password constraints:
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
modifyCacheCluster_authToken :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_authToken = Lens.lens (\ModifyCacheCluster' {authToken} -> authToken) (\s@ModifyCacheCluster' {} a -> s {authToken = a} :: ModifyCacheCluster)

-- | Specifies the strategy to use to update the AUTH token. This parameter
-- must be specified with the @auth-token@ parameter. Possible values:
--
-- -   Rotate
--
-- -   Set
--
-- For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/auth.html Authenticating Users with Redis AUTH>
modifyCacheCluster_authTokenUpdateStrategy :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe AuthTokenUpdateStrategyType)
modifyCacheCluster_authTokenUpdateStrategy = Lens.lens (\ModifyCacheCluster' {authTokenUpdateStrategy} -> authTokenUpdateStrategy) (\s@ModifyCacheCluster' {} a -> s {authTokenUpdateStrategy = a} :: ModifyCacheCluster)

-- | If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
modifyCacheCluster_autoMinorVersionUpgrade :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Bool)
modifyCacheCluster_autoMinorVersionUpgrade = Lens.lens (\ModifyCacheCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyCacheCluster' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyCacheCluster)

-- | A list of cache node IDs to be removed. A node ID is a numeric
-- identifier (0001, 0002, etc.). This parameter is only valid when
-- @NumCacheNodes@ is less than the existing number of cache nodes. The
-- number of cache node IDs supplied in this parameter must match the
-- difference between the existing number of cache nodes in the cluster or
-- pending cache nodes, whichever is greater, and the value of
-- @NumCacheNodes@ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes,
-- and the number of cache nodes in this @ModifyCacheCluster@ call is 5,
-- you must list 2 (7 - 5) cache node IDs to remove.
modifyCacheCluster_cacheNodeIdsToRemove :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe [Prelude.Text])
modifyCacheCluster_cacheNodeIdsToRemove = Lens.lens (\ModifyCacheCluster' {cacheNodeIdsToRemove} -> cacheNodeIdsToRemove) (\s@ModifyCacheCluster' {} a -> s {cacheNodeIdsToRemove = a} :: ModifyCacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | A valid cache node type that you want to scale this cluster up to.
modifyCacheCluster_cacheNodeType :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_cacheNodeType = Lens.lens (\ModifyCacheCluster' {cacheNodeType} -> cacheNodeType) (\s@ModifyCacheCluster' {} a -> s {cacheNodeType = a} :: ModifyCacheCluster)

-- | The name of the cache parameter group to apply to this cluster. This
-- change is asynchronously applied as soon as possible for parameters when
-- the @ApplyImmediately@ parameter is specified as @true@ for this
-- request.
modifyCacheCluster_cacheParameterGroupName :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_cacheParameterGroupName = Lens.lens (\ModifyCacheCluster' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@ModifyCacheCluster' {} a -> s {cacheParameterGroupName = a} :: ModifyCacheCluster)

-- | A list of cache security group names to authorize on this cluster. This
-- change is asynchronously applied as soon as possible.
--
-- You can use this parameter only with clusters that are created outside
-- of an Amazon Virtual Private Cloud (Amazon VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must
-- not be \"Default\".
modifyCacheCluster_cacheSecurityGroupNames :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe [Prelude.Text])
modifyCacheCluster_cacheSecurityGroupNames = Lens.lens (\ModifyCacheCluster' {cacheSecurityGroupNames} -> cacheSecurityGroupNames) (\s@ModifyCacheCluster' {} a -> s {cacheSecurityGroupNames = a} :: ModifyCacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | The upgraded version of the cache engine to be run on the cache nodes.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
-- but you cannot downgrade to an earlier engine version. If you want to
-- use an earlier engine version, you must delete the existing cluster and
-- create it anew with the earlier engine version.
modifyCacheCluster_engineVersion :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_engineVersion = Lens.lens (\ModifyCacheCluster' {engineVersion} -> engineVersion) (\s@ModifyCacheCluster' {} a -> s {engineVersion = a} :: ModifyCacheCluster)

-- | The network type you choose when modifying a cluster, either @ipv4@ |
-- @ipv6@. IPv6 is supported for workloads using Redis engine version 6.2
-- onward or Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
modifyCacheCluster_ipDiscovery :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe IpDiscovery)
modifyCacheCluster_ipDiscovery = Lens.lens (\ModifyCacheCluster' {ipDiscovery} -> ipDiscovery) (\s@ModifyCacheCluster' {} a -> s {ipDiscovery = a} :: ModifyCacheCluster)

-- | Specifies the destination, format and type of the logs.
modifyCacheCluster_logDeliveryConfigurations :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe [LogDeliveryConfigurationRequest])
modifyCacheCluster_logDeliveryConfigurations = Lens.lens (\ModifyCacheCluster' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@ModifyCacheCluster' {} a -> s {logDeliveryConfigurations = a} :: ModifyCacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | This option is only supported on Memcached clusters.
--
-- The list of Availability Zones where the new Memcached cache nodes are
-- created.
--
-- This parameter is only valid when @NumCacheNodes@ in the request is
-- greater than the sum of the number of active cache nodes and the number
-- of cache nodes pending creation (which may be zero). The number of
-- Availability Zones supplied in this list must match the cache nodes
-- being added in this request.
--
-- Scenarios:
--
-- -   __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes.
--     Specify @NumCacheNodes=5@ (3 + 2) and optionally specify two
--     Availability Zones for the two new nodes.
--
-- -   __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation
--     (from the scenario 1 call) and want to add 1 more node. Specify
--     @NumCacheNodes=6@ ((3 + 2) + 1) and optionally specify an
--     Availability Zone for the new node.
--
-- -   __Scenario 3:__ You want to cancel all pending operations. Specify
--     @NumCacheNodes=3@ to cancel all pending operations.
--
-- The Availability Zone placement of nodes pending creation cannot be
-- modified. If you wish to cancel any nodes pending creation, add 0 nodes
-- by setting @NumCacheNodes@ to the number of current nodes.
--
-- If @cross-az@ is specified, existing Memcached nodes remain in their
-- current Availability Zone. Only newly created nodes can be located in
-- different Availability Zones. For guidance on how to move existing
-- Memcached nodes to different Availability Zones, see the __Availability
-- Zone Considerations__ section of
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/mem-ug/CacheNodes.SupportedTypes.html Cache Node Considerations for Memcached>.
--
-- __Impact of new add\/remove requests upon pending requests__
--
-- -   Scenario-1
--
--     -   Pending Action: Delete
--
--     -   New Request: Delete
--
--     -   Result: The new delete, pending or immediate, replaces the
--         pending delete.
--
-- -   Scenario-2
--
--     -   Pending Action: Delete
--
--     -   New Request: Create
--
--     -   Result: The new create, pending or immediate, replaces the
--         pending delete.
--
-- -   Scenario-3
--
--     -   Pending Action: Create
--
--     -   New Request: Delete
--
--     -   Result: The new delete, pending or immediate, replaces the
--         pending create.
--
-- -   Scenario-4
--
--     -   Pending Action: Create
--
--     -   New Request: Create
--
--     -   Result: The new create is added to the pending create.
--
--         __Important:__ If the new create request is __Apply Immediately
--         - Yes__, all creates are performed immediately. If the new
--         create request is __Apply Immediately - No__, all creates are
--         pending.
modifyCacheCluster_newAvailabilityZones :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe [Prelude.Text])
modifyCacheCluster_newAvailabilityZones = Lens.lens (\ModifyCacheCluster' {newAvailabilityZones'} -> newAvailabilityZones') (\s@ModifyCacheCluster' {} a -> s {newAvailabilityZones' = a} :: ModifyCacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications are sent.
--
-- The Amazon SNS topic owner must be same as the cluster owner.
modifyCacheCluster_notificationTopicArn :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_notificationTopicArn = Lens.lens (\ModifyCacheCluster' {notificationTopicArn} -> notificationTopicArn) (\s@ModifyCacheCluster' {} a -> s {notificationTopicArn = a} :: ModifyCacheCluster)

-- | The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is @active@.
--
-- Valid values: @active@ | @inactive@
modifyCacheCluster_notificationTopicStatus :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_notificationTopicStatus = Lens.lens (\ModifyCacheCluster' {notificationTopicStatus} -> notificationTopicStatus) (\s@ModifyCacheCluster' {} a -> s {notificationTopicStatus = a} :: ModifyCacheCluster)

-- | The number of cache nodes that the cluster should have. If the value for
-- @NumCacheNodes@ is greater than the sum of the number of current cache
-- nodes and the number of cache nodes pending creation (which may be
-- zero), more nodes are added. If the value is less than the number of
-- existing cache nodes, nodes are removed. If the value is equal to the
-- number of current cache nodes, any pending add or remove requests are
-- canceled.
--
-- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@
-- parameter to provide the IDs of the specific cache nodes to remove.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
--
-- Adding or removing Memcached cache nodes can be applied immediately or
-- as a pending operation (see @ApplyImmediately@).
--
-- A pending operation to modify the number of cache nodes in a cluster
-- during its maintenance window, whether by adding or removing nodes in
-- accordance with the scale out architecture, is not queued. The
-- customer\'s latest request to add or remove nodes to the cluster
-- overrides any previous pending operations to modify the number of cache
-- nodes in the cluster. For example, a request to remove 2 nodes would
-- override a previous pending operation to remove 3 nodes. Similarly, a
-- request to add 2 nodes would override a previous pending operation to
-- remove 3 nodes and vice versa. As Memcached cache nodes may now be
-- provisioned in different Availability Zones with flexible cache node
-- placement, a request to add nodes does not automatically override a
-- previous pending operation to add nodes. The customer can modify the
-- previous pending operation to add more nodes or explicitly cancel the
-- pending request and retry the new request. To cancel pending operations
-- to modify the number of cache nodes in a cluster, use the
-- @ModifyCacheCluster@ request and set @NumCacheNodes@ equal to the number
-- of cache nodes currently in the cluster.
modifyCacheCluster_numCacheNodes :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Int)
modifyCacheCluster_numCacheNodes = Lens.lens (\ModifyCacheCluster' {numCacheNodes} -> numCacheNodes) (\s@ModifyCacheCluster' {} a -> s {numCacheNodes = a} :: ModifyCacheCluster)

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
modifyCacheCluster_preferredMaintenanceWindow :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_preferredMaintenanceWindow = Lens.lens (\ModifyCacheCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyCacheCluster' {} a -> s {preferredMaintenanceWindow = a} :: ModifyCacheCluster)

-- | Specifies the VPC Security Groups associated with the cluster.
--
-- This parameter can be used only with clusters that are created in an
-- Amazon Virtual Private Cloud (Amazon VPC).
modifyCacheCluster_securityGroupIds :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe [Prelude.Text])
modifyCacheCluster_securityGroupIds = Lens.lens (\ModifyCacheCluster' {securityGroupIds} -> securityGroupIds) (\s@ModifyCacheCluster' {} a -> s {securityGroupIds = a} :: ModifyCacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are
-- turned off.
modifyCacheCluster_snapshotRetentionLimit :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Int)
modifyCacheCluster_snapshotRetentionLimit = Lens.lens (\ModifyCacheCluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@ModifyCacheCluster' {} a -> s {snapshotRetentionLimit = a} :: ModifyCacheCluster)

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your cluster.
modifyCacheCluster_snapshotWindow :: Lens.Lens' ModifyCacheCluster (Prelude.Maybe Prelude.Text)
modifyCacheCluster_snapshotWindow = Lens.lens (\ModifyCacheCluster' {snapshotWindow} -> snapshotWindow) (\s@ModifyCacheCluster' {} a -> s {snapshotWindow = a} :: ModifyCacheCluster)

-- | The cluster identifier. This value is stored as a lowercase string.
modifyCacheCluster_cacheClusterId :: Lens.Lens' ModifyCacheCluster Prelude.Text
modifyCacheCluster_cacheClusterId = Lens.lens (\ModifyCacheCluster' {cacheClusterId} -> cacheClusterId) (\s@ModifyCacheCluster' {} a -> s {cacheClusterId = a} :: ModifyCacheCluster)

instance Core.AWSRequest ModifyCacheCluster where
  type
    AWSResponse ModifyCacheCluster =
      ModifyCacheClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyCacheClusterResult"
      ( \s h x ->
          ModifyCacheClusterResponse'
            Prelude.<$> (x Data..@? "CacheCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyCacheCluster where
  hashWithSalt _salt ModifyCacheCluster' {..} =
    _salt `Prelude.hashWithSalt` aZMode
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` authToken
      `Prelude.hashWithSalt` authTokenUpdateStrategy
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` cacheNodeIdsToRemove
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` cacheParameterGroupName
      `Prelude.hashWithSalt` cacheSecurityGroupNames
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` ipDiscovery
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` newAvailabilityZones'
      `Prelude.hashWithSalt` notificationTopicArn
      `Prelude.hashWithSalt` notificationTopicStatus
      `Prelude.hashWithSalt` numCacheNodes
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` cacheClusterId

instance Prelude.NFData ModifyCacheCluster where
  rnf ModifyCacheCluster' {..} =
    Prelude.rnf aZMode
      `Prelude.seq` Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf authToken
      `Prelude.seq` Prelude.rnf authTokenUpdateStrategy
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf cacheNodeIdsToRemove
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf cacheParameterGroupName
      `Prelude.seq` Prelude.rnf cacheSecurityGroupNames
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf ipDiscovery
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf newAvailabilityZones'
      `Prelude.seq` Prelude.rnf notificationTopicArn
      `Prelude.seq` Prelude.rnf notificationTopicStatus
      `Prelude.seq` Prelude.rnf numCacheNodes
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf
        snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf cacheClusterId

instance Data.ToHeaders ModifyCacheCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyCacheCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCacheCluster where
  toQuery ModifyCacheCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyCacheCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "AZMode" Data.=: aZMode,
        "ApplyImmediately" Data.=: applyImmediately,
        "AuthToken" Data.=: authToken,
        "AuthTokenUpdateStrategy"
          Data.=: authTokenUpdateStrategy,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "CacheNodeIdsToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "CacheNodeId"
                Prelude.<$> cacheNodeIdsToRemove
            ),
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
        "NewAvailabilityZones"
          Data.=: Data.toQuery
            ( Data.toQueryList "PreferredAvailabilityZone"
                Prelude.<$> newAvailabilityZones'
            ),
        "NotificationTopicArn" Data.=: notificationTopicArn,
        "NotificationTopicStatus"
          Data.=: notificationTopicStatus,
        "NumCacheNodes" Data.=: numCacheNodes,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "SecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "SecurityGroupId"
                Prelude.<$> securityGroupIds
            ),
        "SnapshotRetentionLimit"
          Data.=: snapshotRetentionLimit,
        "SnapshotWindow" Data.=: snapshotWindow,
        "CacheClusterId" Data.=: cacheClusterId
      ]

-- | /See:/ 'newModifyCacheClusterResponse' smart constructor.
data ModifyCacheClusterResponse = ModifyCacheClusterResponse'
  { cacheCluster :: Prelude.Maybe CacheCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCacheClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheCluster', 'modifyCacheClusterResponse_cacheCluster' - Undocumented member.
--
-- 'httpStatus', 'modifyCacheClusterResponse_httpStatus' - The response's http status code.
newModifyCacheClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyCacheClusterResponse
newModifyCacheClusterResponse pHttpStatus_ =
  ModifyCacheClusterResponse'
    { cacheCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyCacheClusterResponse_cacheCluster :: Lens.Lens' ModifyCacheClusterResponse (Prelude.Maybe CacheCluster)
modifyCacheClusterResponse_cacheCluster = Lens.lens (\ModifyCacheClusterResponse' {cacheCluster} -> cacheCluster) (\s@ModifyCacheClusterResponse' {} a -> s {cacheCluster = a} :: ModifyCacheClusterResponse)

-- | The response's http status code.
modifyCacheClusterResponse_httpStatus :: Lens.Lens' ModifyCacheClusterResponse Prelude.Int
modifyCacheClusterResponse_httpStatus = Lens.lens (\ModifyCacheClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyCacheClusterResponse' {} a -> s {httpStatus = a} :: ModifyCacheClusterResponse)

instance Prelude.NFData ModifyCacheClusterResponse where
  rnf ModifyCacheClusterResponse' {..} =
    Prelude.rnf cacheCluster
      `Prelude.seq` Prelude.rnf httpStatus
