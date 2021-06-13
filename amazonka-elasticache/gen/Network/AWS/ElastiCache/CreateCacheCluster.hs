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
-- Module      : Network.AWS.ElastiCache.CreateCacheCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cluster. All nodes in the cluster run the same
-- protocol-compliant cache engine software, either Memcached or Redis.
--
-- This operation is not supported for Redis (cluster mode enabled)
-- clusters.
module Network.AWS.ElastiCache.CreateCacheCluster
  ( -- * Creating a Request
    CreateCacheCluster (..),
    newCreateCacheCluster,

    -- * Request Lenses
    createCacheCluster_securityGroupIds,
    createCacheCluster_preferredAvailabilityZones,
    createCacheCluster_numCacheNodes,
    createCacheCluster_cacheSecurityGroupNames,
    createCacheCluster_replicationGroupId,
    createCacheCluster_snapshotWindow,
    createCacheCluster_preferredOutpostArns,
    createCacheCluster_outpostMode,
    createCacheCluster_cacheParameterGroupName,
    createCacheCluster_snapshotRetentionLimit,
    createCacheCluster_snapshotArns,
    createCacheCluster_preferredAvailabilityZone,
    createCacheCluster_cacheSubnetGroupName,
    createCacheCluster_engineVersion,
    createCacheCluster_preferredMaintenanceWindow,
    createCacheCluster_cacheNodeType,
    createCacheCluster_tags,
    createCacheCluster_notificationTopicArn,
    createCacheCluster_port,
    createCacheCluster_engine,
    createCacheCluster_preferredOutpostArn,
    createCacheCluster_snapshotName,
    createCacheCluster_authToken,
    createCacheCluster_aZMode,
    createCacheCluster_autoMinorVersionUpgrade,
    createCacheCluster_cacheClusterId,

    -- * Destructuring the Response
    CreateCacheClusterResponse (..),
    newCreateCacheClusterResponse,

    -- * Response Lenses
    createCacheClusterResponse_cacheCluster,
    createCacheClusterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a CreateCacheCluster operation.
--
-- /See:/ 'newCreateCacheCluster' smart constructor.
data CreateCacheCluster = CreateCacheCluster'
  { -- | One or more VPC security groups associated with the cluster.
    --
    -- Use this parameter only when you are creating a cluster in an Amazon
    -- Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of the Availability Zones in which cache nodes are created. The
    -- order of the zones in the list is not important.
    --
    -- This option is only supported on Memcached.
    --
    -- If you are creating your cluster in an Amazon VPC (recommended) you can
    -- only locate nodes in Availability Zones that are associated with the
    -- subnets in the selected subnet group.
    --
    -- The number of Availability Zones listed must equal the value of
    -- @NumCacheNodes@.
    --
    -- If you want all the nodes in the same Availability Zone, use
    -- @PreferredAvailabilityZone@ instead, or repeat the Availability Zone
    -- multiple times in the list.
    --
    -- Default: System chosen Availability Zones.
    preferredAvailabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The initial number of cache nodes that the cluster has.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 20.
    --
    -- If you need more than 20 nodes for your Memcached cluster, please fill
    -- out the ElastiCache Limit Increase Request form at
    -- <http://aws.amazon.com/contact-us/elasticache-node-limit-request/>.
    numCacheNodes :: Prelude.Maybe Prelude.Int,
    -- | A list of security group names to associate with this cluster.
    --
    -- Use this parameter only when you are creating a cluster outside of an
    -- Amazon Virtual Private Cloud (Amazon VPC).
    cacheSecurityGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the replication group to which this cluster should belong. If
    -- this parameter is specified, the cluster is added to the specified
    -- replication group as a read replica; otherwise, the cluster is a
    -- standalone primary that is not part of any replication group.
    --
    -- If the specified replication group is Multi-AZ enabled and the
    -- Availability Zone is not specified, the cluster is created in
    -- Availability Zones that provide the best spread of read replicas across
    -- Availability Zones.
    --
    -- This parameter is only valid if the @Engine@ parameter is @redis@.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of your node group (shard).
    --
    -- Example: @05:00-09:00@
    --
    -- If you do not specify this parameter, ElastiCache automatically chooses
    -- an appropriate time range.
    --
    -- This parameter is only valid if the @Engine@ parameter is @redis@.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The outpost ARNs in which the cache cluster is created.
    preferredOutpostArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether the nodes in the cluster are created in a single
    -- outpost or across multiple outposts.
    outpostMode :: Prelude.Maybe OutpostMode,
    -- | The name of the parameter group to associate with this cluster. If this
    -- argument is omitted, the default parameter group for the specified
    -- engine is used. You cannot use any parameter group which has
    -- @cluster-enabled=\'yes\'@ when creating a cluster.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which ElastiCache retains automatic snapshots
    -- before deleting them. For example, if you set @SnapshotRetentionLimit@
    -- to 5, a snapshot taken today is retained for 5 days before being
    -- deleted.
    --
    -- This parameter is only valid if the @Engine@ parameter is @redis@.
    --
    -- Default: 0 (i.e., automatic backups are disabled for this cache
    -- cluster).
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | A single-element string list containing an Amazon Resource Name (ARN)
    -- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
    -- The snapshot file is used to populate the node group (shard). The Amazon
    -- S3 object name in the ARN cannot contain any commas.
    --
    -- This parameter is only valid if the @Engine@ parameter is @redis@.
    --
    -- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
    snapshotArns :: Prelude.Maybe [Prelude.Text],
    -- | The EC2 Availability Zone in which the cluster is created.
    --
    -- All nodes belonging to this cluster are placed in the preferred
    -- Availability Zone. If you want to create your nodes across multiple
    -- Availability Zones, use @PreferredAvailabilityZones@.
    --
    -- Default: System chosen Availability Zone.
    preferredAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The name of the subnet group to be used for the cluster.
    --
    -- Use this parameter only when you are creating a cluster in an Amazon
    -- Virtual Private Cloud (Amazon VPC).
    --
    -- If you\'re going to launch your cluster in an Amazon VPC, you need to
    -- create a subnet group before you start creating a cluster. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups>.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the cache engine to be used for this cluster. To
    -- view the supported cache engine versions, use the
    -- DescribeCacheEngineVersions operation.
    --
    -- __Important:__ You can upgrade to a newer engine version (see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
    -- but you cannot downgrade to an earlier engine version. If you want to
    -- use an earlier engine version, you must delete the existing cluster or
    -- replication group and create it anew with the earlier engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period. Valid values for @ddd@ are:
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The compute and memory capacity of the nodes in the node group (shard).
    --
    -- The following node types are supported by ElastiCache. Generally
    -- speaking, the current generation types provide more memory and
    -- computational power at lower cost when compared to their equivalent
    -- previous generation counterparts.
    --
    -- -   General purpose:
    --
    --     -   Current generation:
    --
    --         __M6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
    --         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
    --         @cache.m6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
    --         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
    --         @cache.m5.24xlarge@
    --
    --         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
    --         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
    --
    --         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
    --         @cache.t3.medium@
    --
    --         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
    --         @cache.t2.medium@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __T1 node types:__ @cache.t1.micro@
    --
    --         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
    --         @cache.m1.large@, @cache.m1.xlarge@
    --
    --         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
    --         @cache.m3.xlarge@, @cache.m3.2xlarge@
    --
    -- -   Compute optimized:
    --
    --     -   Previous generation: (not recommended)
    --
    --         __C1 node types:__ @cache.c1.xlarge@
    --
    -- -   Memory optimized:
    --
    --     -   Current generation:
    --
    --         __R6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
    --         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
    --         @cache.r6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
    --         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
    --         @cache.r5.24xlarge@
    --
    --         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
    --         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
    --         @cache.r4.16xlarge@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
    --         @cache.m2.4xlarge@
    --
    --         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
    --         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
    --
    -- __Additional node type info__
    --
    -- -   All current generation instance types are created in Amazon VPC by
    --     default.
    --
    -- -   Redis append-only files (AOF) are not supported for T1 or T2
    --     instances.
    --
    -- -   Redis Multi-AZ with automatic failover is not supported on T1
    --     instances.
    --
    -- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
    --     supported on Redis version 2.8.22 and later.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | A list of cost allocation tags to be added to this resource.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- (SNS) topic to which notifications are sent.
    --
    -- The Amazon SNS topic owner must be the same as the cluster owner.
    notificationTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The port number on which each of the cache nodes accepts connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | The name of the cache engine to be used for this cluster.
    --
    -- Valid values for this parameter are: @memcached@ | @redis@
    engine :: Prelude.Maybe Prelude.Text,
    -- | The outpost ARN in which the cache cluster is created.
    preferredOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a Redis snapshot from which to restore data into the new
    -- node group (shard). The snapshot status changes to @restoring@ while the
    -- new node group (shard) is being created.
    --
    -- This parameter is only valid if the @Engine@ parameter is @redis@.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | __Reserved parameter.__ The password used to access a password protected
    -- server.
    --
    -- Password constraints:
    --
    -- -   Must be only printable ASCII characters.
    --
    -- -   Must be at least 16 characters and no more than 128 characters in
    --     length.
    --
    -- -   The only permitted printable special characters are !, &, #, $, ^,
    --     \<, >, and -. Other printable special characters cannot be used in
    --     the AUTH token.
    --
    -- For more information, see <http://redis.io/commands/AUTH AUTH password>
    -- at http:\/\/redis.io\/commands\/AUTH.
    authToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the nodes in this Memcached cluster are created in a
    -- single Availability Zone or created across multiple Availability Zones
    -- in the cluster\'s region.
    --
    -- This parameter is only supported for Memcached clusters.
    --
    -- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified,
    -- ElastiCache assumes @single-az@ mode.
    aZMode :: Prelude.Maybe AZMode,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The node group (shard) identifier. This parameter is stored as a
    -- lowercase string.
    --
    -- __Constraints:__
    --
    -- -   A name must contain from 1 to 50 alphanumeric characters or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   A name cannot end with a hyphen or contain two consecutive hyphens.
    cacheClusterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCacheCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createCacheCluster_securityGroupIds' - One or more VPC security groups associated with the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon
-- Virtual Private Cloud (Amazon VPC).
--
-- 'preferredAvailabilityZones', 'createCacheCluster_preferredAvailabilityZones' - A list of the Availability Zones in which cache nodes are created. The
-- order of the zones in the list is not important.
--
-- This option is only supported on Memcached.
--
-- If you are creating your cluster in an Amazon VPC (recommended) you can
-- only locate nodes in Availability Zones that are associated with the
-- subnets in the selected subnet group.
--
-- The number of Availability Zones listed must equal the value of
-- @NumCacheNodes@.
--
-- If you want all the nodes in the same Availability Zone, use
-- @PreferredAvailabilityZone@ instead, or repeat the Availability Zone
-- multiple times in the list.
--
-- Default: System chosen Availability Zones.
--
-- 'numCacheNodes', 'createCacheCluster_numCacheNodes' - The initial number of cache nodes that the cluster has.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
--
-- If you need more than 20 nodes for your Memcached cluster, please fill
-- out the ElastiCache Limit Increase Request form at
-- <http://aws.amazon.com/contact-us/elasticache-node-limit-request/>.
--
-- 'cacheSecurityGroupNames', 'createCacheCluster_cacheSecurityGroupNames' - A list of security group names to associate with this cluster.
--
-- Use this parameter only when you are creating a cluster outside of an
-- Amazon Virtual Private Cloud (Amazon VPC).
--
-- 'replicationGroupId', 'createCacheCluster_replicationGroupId' - The ID of the replication group to which this cluster should belong. If
-- this parameter is specified, the cluster is added to the specified
-- replication group as a read replica; otherwise, the cluster is a
-- standalone primary that is not part of any replication group.
--
-- If the specified replication group is Multi-AZ enabled and the
-- Availability Zone is not specified, the cluster is created in
-- Availability Zones that provide the best spread of read replicas across
-- Availability Zones.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- 'snapshotWindow', 'createCacheCluster_snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- 'preferredOutpostArns', 'createCacheCluster_preferredOutpostArns' - The outpost ARNs in which the cache cluster is created.
--
-- 'outpostMode', 'createCacheCluster_outpostMode' - Specifies whether the nodes in the cluster are created in a single
-- outpost or across multiple outposts.
--
-- 'cacheParameterGroupName', 'createCacheCluster_cacheParameterGroupName' - The name of the parameter group to associate with this cluster. If this
-- argument is omitted, the default parameter group for the specified
-- engine is used. You cannot use any parameter group which has
-- @cluster-enabled=\'yes\'@ when creating a cluster.
--
-- 'snapshotRetentionLimit', 'createCacheCluster_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic snapshots
-- before deleting them. For example, if you set @SnapshotRetentionLimit@
-- to 5, a snapshot taken today is retained for 5 days before being
-- deleted.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache
-- cluster).
--
-- 'snapshotArns', 'createCacheCluster_snapshotArns' - A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file is used to populate the node group (shard). The Amazon
-- S3 object name in the ARN cannot contain any commas.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
--
-- 'preferredAvailabilityZone', 'createCacheCluster_preferredAvailabilityZone' - The EC2 Availability Zone in which the cluster is created.
--
-- All nodes belonging to this cluster are placed in the preferred
-- Availability Zone. If you want to create your nodes across multiple
-- Availability Zones, use @PreferredAvailabilityZones@.
--
-- Default: System chosen Availability Zone.
--
-- 'cacheSubnetGroupName', 'createCacheCluster_cacheSubnetGroupName' - The name of the subnet group to be used for the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon
-- Virtual Private Cloud (Amazon VPC).
--
-- If you\'re going to launch your cluster in an Amazon VPC, you need to
-- create a subnet group before you start creating a cluster. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups>.
--
-- 'engineVersion', 'createCacheCluster_engineVersion' - The version number of the cache engine to be used for this cluster. To
-- view the supported cache engine versions, use the
-- DescribeCacheEngineVersions operation.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
-- but you cannot downgrade to an earlier engine version. If you want to
-- use an earlier engine version, you must delete the existing cluster or
-- replication group and create it anew with the earlier engine version.
--
-- 'preferredMaintenanceWindow', 'createCacheCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for @ddd@ are:
--
-- 'cacheNodeType', 'createCacheCluster_cacheNodeType' - The compute and memory capacity of the nodes in the node group (shard).
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
--
-- 'tags', 'createCacheCluster_tags' - A list of cost allocation tags to be added to this resource.
--
-- 'notificationTopicArn', 'createCacheCluster_notificationTopicArn' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications are sent.
--
-- The Amazon SNS topic owner must be the same as the cluster owner.
--
-- 'port', 'createCacheCluster_port' - The port number on which each of the cache nodes accepts connections.
--
-- 'engine', 'createCacheCluster_engine' - The name of the cache engine to be used for this cluster.
--
-- Valid values for this parameter are: @memcached@ | @redis@
--
-- 'preferredOutpostArn', 'createCacheCluster_preferredOutpostArn' - The outpost ARN in which the cache cluster is created.
--
-- 'snapshotName', 'createCacheCluster_snapshotName' - The name of a Redis snapshot from which to restore data into the new
-- node group (shard). The snapshot status changes to @restoring@ while the
-- new node group (shard) is being created.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- 'authToken', 'createCacheCluster_authToken' - __Reserved parameter.__ The password used to access a password protected
-- server.
--
-- Password constraints:
--
-- -   Must be only printable ASCII characters.
--
-- -   Must be at least 16 characters and no more than 128 characters in
--     length.
--
-- -   The only permitted printable special characters are !, &, #, $, ^,
--     \<, >, and -. Other printable special characters cannot be used in
--     the AUTH token.
--
-- For more information, see <http://redis.io/commands/AUTH AUTH password>
-- at http:\/\/redis.io\/commands\/AUTH.
--
-- 'aZMode', 'createCacheCluster_aZMode' - Specifies whether the nodes in this Memcached cluster are created in a
-- single Availability Zone or created across multiple Availability Zones
-- in the cluster\'s region.
--
-- This parameter is only supported for Memcached clusters.
--
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified,
-- ElastiCache assumes @single-az@ mode.
--
-- 'autoMinorVersionUpgrade', 'createCacheCluster_autoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- 'cacheClusterId', 'createCacheCluster_cacheClusterId' - The node group (shard) identifier. This parameter is stored as a
-- lowercase string.
--
-- __Constraints:__
--
-- -   A name must contain from 1 to 50 alphanumeric characters or hyphens.
--
-- -   The first character must be a letter.
--
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
newCreateCacheCluster ::
  -- | 'cacheClusterId'
  Prelude.Text ->
  CreateCacheCluster
newCreateCacheCluster pCacheClusterId_ =
  CreateCacheCluster'
    { securityGroupIds =
        Prelude.Nothing,
      preferredAvailabilityZones = Prelude.Nothing,
      numCacheNodes = Prelude.Nothing,
      cacheSecurityGroupNames = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      preferredOutpostArns = Prelude.Nothing,
      outpostMode = Prelude.Nothing,
      cacheParameterGroupName = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snapshotArns = Prelude.Nothing,
      preferredAvailabilityZone = Prelude.Nothing,
      cacheSubnetGroupName = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      tags = Prelude.Nothing,
      notificationTopicArn = Prelude.Nothing,
      port = Prelude.Nothing,
      engine = Prelude.Nothing,
      preferredOutpostArn = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      authToken = Prelude.Nothing,
      aZMode = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      cacheClusterId = pCacheClusterId_
    }

-- | One or more VPC security groups associated with the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon
-- Virtual Private Cloud (Amazon VPC).
createCacheCluster_securityGroupIds :: Lens.Lens' CreateCacheCluster (Prelude.Maybe [Prelude.Text])
createCacheCluster_securityGroupIds = Lens.lens (\CreateCacheCluster' {securityGroupIds} -> securityGroupIds) (\s@CreateCacheCluster' {} a -> s {securityGroupIds = a} :: CreateCacheCluster) Prelude.. Lens.mapping Lens._Coerce

-- | A list of the Availability Zones in which cache nodes are created. The
-- order of the zones in the list is not important.
--
-- This option is only supported on Memcached.
--
-- If you are creating your cluster in an Amazon VPC (recommended) you can
-- only locate nodes in Availability Zones that are associated with the
-- subnets in the selected subnet group.
--
-- The number of Availability Zones listed must equal the value of
-- @NumCacheNodes@.
--
-- If you want all the nodes in the same Availability Zone, use
-- @PreferredAvailabilityZone@ instead, or repeat the Availability Zone
-- multiple times in the list.
--
-- Default: System chosen Availability Zones.
createCacheCluster_preferredAvailabilityZones :: Lens.Lens' CreateCacheCluster (Prelude.Maybe [Prelude.Text])
createCacheCluster_preferredAvailabilityZones = Lens.lens (\CreateCacheCluster' {preferredAvailabilityZones} -> preferredAvailabilityZones) (\s@CreateCacheCluster' {} a -> s {preferredAvailabilityZones = a} :: CreateCacheCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The initial number of cache nodes that the cluster has.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
--
-- If you need more than 20 nodes for your Memcached cluster, please fill
-- out the ElastiCache Limit Increase Request form at
-- <http://aws.amazon.com/contact-us/elasticache-node-limit-request/>.
createCacheCluster_numCacheNodes :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Int)
createCacheCluster_numCacheNodes = Lens.lens (\CreateCacheCluster' {numCacheNodes} -> numCacheNodes) (\s@CreateCacheCluster' {} a -> s {numCacheNodes = a} :: CreateCacheCluster)

-- | A list of security group names to associate with this cluster.
--
-- Use this parameter only when you are creating a cluster outside of an
-- Amazon Virtual Private Cloud (Amazon VPC).
createCacheCluster_cacheSecurityGroupNames :: Lens.Lens' CreateCacheCluster (Prelude.Maybe [Prelude.Text])
createCacheCluster_cacheSecurityGroupNames = Lens.lens (\CreateCacheCluster' {cacheSecurityGroupNames} -> cacheSecurityGroupNames) (\s@CreateCacheCluster' {} a -> s {cacheSecurityGroupNames = a} :: CreateCacheCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the replication group to which this cluster should belong. If
-- this parameter is specified, the cluster is added to the specified
-- replication group as a read replica; otherwise, the cluster is a
-- standalone primary that is not part of any replication group.
--
-- If the specified replication group is Multi-AZ enabled and the
-- Availability Zone is not specified, the cluster is created in
-- Availability Zones that provide the best spread of read replicas across
-- Availability Zones.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
createCacheCluster_replicationGroupId :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_replicationGroupId = Lens.lens (\CreateCacheCluster' {replicationGroupId} -> replicationGroupId) (\s@CreateCacheCluster' {} a -> s {replicationGroupId = a} :: CreateCacheCluster)

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
createCacheCluster_snapshotWindow :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_snapshotWindow = Lens.lens (\CreateCacheCluster' {snapshotWindow} -> snapshotWindow) (\s@CreateCacheCluster' {} a -> s {snapshotWindow = a} :: CreateCacheCluster)

-- | The outpost ARNs in which the cache cluster is created.
createCacheCluster_preferredOutpostArns :: Lens.Lens' CreateCacheCluster (Prelude.Maybe [Prelude.Text])
createCacheCluster_preferredOutpostArns = Lens.lens (\CreateCacheCluster' {preferredOutpostArns} -> preferredOutpostArns) (\s@CreateCacheCluster' {} a -> s {preferredOutpostArns = a} :: CreateCacheCluster) Prelude.. Lens.mapping Lens._Coerce

-- | Specifies whether the nodes in the cluster are created in a single
-- outpost or across multiple outposts.
createCacheCluster_outpostMode :: Lens.Lens' CreateCacheCluster (Prelude.Maybe OutpostMode)
createCacheCluster_outpostMode = Lens.lens (\CreateCacheCluster' {outpostMode} -> outpostMode) (\s@CreateCacheCluster' {} a -> s {outpostMode = a} :: CreateCacheCluster)

-- | The name of the parameter group to associate with this cluster. If this
-- argument is omitted, the default parameter group for the specified
-- engine is used. You cannot use any parameter group which has
-- @cluster-enabled=\'yes\'@ when creating a cluster.
createCacheCluster_cacheParameterGroupName :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_cacheParameterGroupName = Lens.lens (\CreateCacheCluster' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@CreateCacheCluster' {} a -> s {cacheParameterGroupName = a} :: CreateCacheCluster)

-- | The number of days for which ElastiCache retains automatic snapshots
-- before deleting them. For example, if you set @SnapshotRetentionLimit@
-- to 5, a snapshot taken today is retained for 5 days before being
-- deleted.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache
-- cluster).
createCacheCluster_snapshotRetentionLimit :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Int)
createCacheCluster_snapshotRetentionLimit = Lens.lens (\CreateCacheCluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@CreateCacheCluster' {} a -> s {snapshotRetentionLimit = a} :: CreateCacheCluster)

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file is used to populate the node group (shard). The Amazon
-- S3 object name in the ARN cannot contain any commas.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
createCacheCluster_snapshotArns :: Lens.Lens' CreateCacheCluster (Prelude.Maybe [Prelude.Text])
createCacheCluster_snapshotArns = Lens.lens (\CreateCacheCluster' {snapshotArns} -> snapshotArns) (\s@CreateCacheCluster' {} a -> s {snapshotArns = a} :: CreateCacheCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The EC2 Availability Zone in which the cluster is created.
--
-- All nodes belonging to this cluster are placed in the preferred
-- Availability Zone. If you want to create your nodes across multiple
-- Availability Zones, use @PreferredAvailabilityZones@.
--
-- Default: System chosen Availability Zone.
createCacheCluster_preferredAvailabilityZone :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_preferredAvailabilityZone = Lens.lens (\CreateCacheCluster' {preferredAvailabilityZone} -> preferredAvailabilityZone) (\s@CreateCacheCluster' {} a -> s {preferredAvailabilityZone = a} :: CreateCacheCluster)

-- | The name of the subnet group to be used for the cluster.
--
-- Use this parameter only when you are creating a cluster in an Amazon
-- Virtual Private Cloud (Amazon VPC).
--
-- If you\'re going to launch your cluster in an Amazon VPC, you need to
-- create a subnet group before you start creating a cluster. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups>.
createCacheCluster_cacheSubnetGroupName :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_cacheSubnetGroupName = Lens.lens (\CreateCacheCluster' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@CreateCacheCluster' {} a -> s {cacheSubnetGroupName = a} :: CreateCacheCluster)

-- | The version number of the cache engine to be used for this cluster. To
-- view the supported cache engine versions, use the
-- DescribeCacheEngineVersions operation.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>),
-- but you cannot downgrade to an earlier engine version. If you want to
-- use an earlier engine version, you must delete the existing cluster or
-- replication group and create it anew with the earlier engine version.
createCacheCluster_engineVersion :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_engineVersion = Lens.lens (\CreateCacheCluster' {engineVersion} -> engineVersion) (\s@CreateCacheCluster' {} a -> s {engineVersion = a} :: CreateCacheCluster)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for @ddd@ are:
createCacheCluster_preferredMaintenanceWindow :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_preferredMaintenanceWindow = Lens.lens (\CreateCacheCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateCacheCluster' {} a -> s {preferredMaintenanceWindow = a} :: CreateCacheCluster)

-- | The compute and memory capacity of the nodes in the node group (shard).
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
createCacheCluster_cacheNodeType :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_cacheNodeType = Lens.lens (\CreateCacheCluster' {cacheNodeType} -> cacheNodeType) (\s@CreateCacheCluster' {} a -> s {cacheNodeType = a} :: CreateCacheCluster)

-- | A list of cost allocation tags to be added to this resource.
createCacheCluster_tags :: Lens.Lens' CreateCacheCluster (Prelude.Maybe [Tag])
createCacheCluster_tags = Lens.lens (\CreateCacheCluster' {tags} -> tags) (\s@CreateCacheCluster' {} a -> s {tags = a} :: CreateCacheCluster) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications are sent.
--
-- The Amazon SNS topic owner must be the same as the cluster owner.
createCacheCluster_notificationTopicArn :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_notificationTopicArn = Lens.lens (\CreateCacheCluster' {notificationTopicArn} -> notificationTopicArn) (\s@CreateCacheCluster' {} a -> s {notificationTopicArn = a} :: CreateCacheCluster)

-- | The port number on which each of the cache nodes accepts connections.
createCacheCluster_port :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Int)
createCacheCluster_port = Lens.lens (\CreateCacheCluster' {port} -> port) (\s@CreateCacheCluster' {} a -> s {port = a} :: CreateCacheCluster)

-- | The name of the cache engine to be used for this cluster.
--
-- Valid values for this parameter are: @memcached@ | @redis@
createCacheCluster_engine :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_engine = Lens.lens (\CreateCacheCluster' {engine} -> engine) (\s@CreateCacheCluster' {} a -> s {engine = a} :: CreateCacheCluster)

-- | The outpost ARN in which the cache cluster is created.
createCacheCluster_preferredOutpostArn :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_preferredOutpostArn = Lens.lens (\CreateCacheCluster' {preferredOutpostArn} -> preferredOutpostArn) (\s@CreateCacheCluster' {} a -> s {preferredOutpostArn = a} :: CreateCacheCluster)

-- | The name of a Redis snapshot from which to restore data into the new
-- node group (shard). The snapshot status changes to @restoring@ while the
-- new node group (shard) is being created.
--
-- This parameter is only valid if the @Engine@ parameter is @redis@.
createCacheCluster_snapshotName :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_snapshotName = Lens.lens (\CreateCacheCluster' {snapshotName} -> snapshotName) (\s@CreateCacheCluster' {} a -> s {snapshotName = a} :: CreateCacheCluster)

-- | __Reserved parameter.__ The password used to access a password protected
-- server.
--
-- Password constraints:
--
-- -   Must be only printable ASCII characters.
--
-- -   Must be at least 16 characters and no more than 128 characters in
--     length.
--
-- -   The only permitted printable special characters are !, &, #, $, ^,
--     \<, >, and -. Other printable special characters cannot be used in
--     the AUTH token.
--
-- For more information, see <http://redis.io/commands/AUTH AUTH password>
-- at http:\/\/redis.io\/commands\/AUTH.
createCacheCluster_authToken :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Text)
createCacheCluster_authToken = Lens.lens (\CreateCacheCluster' {authToken} -> authToken) (\s@CreateCacheCluster' {} a -> s {authToken = a} :: CreateCacheCluster)

-- | Specifies whether the nodes in this Memcached cluster are created in a
-- single Availability Zone or created across multiple Availability Zones
-- in the cluster\'s region.
--
-- This parameter is only supported for Memcached clusters.
--
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified,
-- ElastiCache assumes @single-az@ mode.
createCacheCluster_aZMode :: Lens.Lens' CreateCacheCluster (Prelude.Maybe AZMode)
createCacheCluster_aZMode = Lens.lens (\CreateCacheCluster' {aZMode} -> aZMode) (\s@CreateCacheCluster' {} a -> s {aZMode = a} :: CreateCacheCluster)

-- | This parameter is currently disabled.
createCacheCluster_autoMinorVersionUpgrade :: Lens.Lens' CreateCacheCluster (Prelude.Maybe Prelude.Bool)
createCacheCluster_autoMinorVersionUpgrade = Lens.lens (\CreateCacheCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateCacheCluster' {} a -> s {autoMinorVersionUpgrade = a} :: CreateCacheCluster)

-- | The node group (shard) identifier. This parameter is stored as a
-- lowercase string.
--
-- __Constraints:__
--
-- -   A name must contain from 1 to 50 alphanumeric characters or hyphens.
--
-- -   The first character must be a letter.
--
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
createCacheCluster_cacheClusterId :: Lens.Lens' CreateCacheCluster Prelude.Text
createCacheCluster_cacheClusterId = Lens.lens (\CreateCacheCluster' {cacheClusterId} -> cacheClusterId) (\s@CreateCacheCluster' {} a -> s {cacheClusterId = a} :: CreateCacheCluster)

instance Core.AWSRequest CreateCacheCluster where
  type
    AWSResponse CreateCacheCluster =
      CreateCacheClusterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateCacheClusterResult"
      ( \s h x ->
          CreateCacheClusterResponse'
            Prelude.<$> (x Core..@? "CacheCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCacheCluster

instance Prelude.NFData CreateCacheCluster

instance Core.ToHeaders CreateCacheCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateCacheCluster where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCacheCluster where
  toQuery CreateCacheCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateCacheCluster" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "SecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "SecurityGroupId"
                Prelude.<$> securityGroupIds
            ),
        "PreferredAvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "PreferredAvailabilityZone"
                Prelude.<$> preferredAvailabilityZones
            ),
        "NumCacheNodes" Core.=: numCacheNodes,
        "CacheSecurityGroupNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "CacheSecurityGroupName"
                Prelude.<$> cacheSecurityGroupNames
            ),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "SnapshotWindow" Core.=: snapshotWindow,
        "PreferredOutpostArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "PreferredOutpostArn"
                Prelude.<$> preferredOutpostArns
            ),
        "OutpostMode" Core.=: outpostMode,
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName,
        "SnapshotRetentionLimit"
          Core.=: snapshotRetentionLimit,
        "SnapshotArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "SnapshotArn"
                Prelude.<$> snapshotArns
            ),
        "PreferredAvailabilityZone"
          Core.=: preferredAvailabilityZone,
        "CacheSubnetGroupName" Core.=: cacheSubnetGroupName,
        "EngineVersion" Core.=: engineVersion,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "CacheNodeType" Core.=: cacheNodeType,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "NotificationTopicArn" Core.=: notificationTopicArn,
        "Port" Core.=: port,
        "Engine" Core.=: engine,
        "PreferredOutpostArn" Core.=: preferredOutpostArn,
        "SnapshotName" Core.=: snapshotName,
        "AuthToken" Core.=: authToken,
        "AZMode" Core.=: aZMode,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "CacheClusterId" Core.=: cacheClusterId
      ]

-- | /See:/ 'newCreateCacheClusterResponse' smart constructor.
data CreateCacheClusterResponse = CreateCacheClusterResponse'
  { cacheCluster :: Prelude.Maybe CacheCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCacheClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheCluster', 'createCacheClusterResponse_cacheCluster' - Undocumented member.
--
-- 'httpStatus', 'createCacheClusterResponse_httpStatus' - The response's http status code.
newCreateCacheClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCacheClusterResponse
newCreateCacheClusterResponse pHttpStatus_ =
  CreateCacheClusterResponse'
    { cacheCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createCacheClusterResponse_cacheCluster :: Lens.Lens' CreateCacheClusterResponse (Prelude.Maybe CacheCluster)
createCacheClusterResponse_cacheCluster = Lens.lens (\CreateCacheClusterResponse' {cacheCluster} -> cacheCluster) (\s@CreateCacheClusterResponse' {} a -> s {cacheCluster = a} :: CreateCacheClusterResponse)

-- | The response's http status code.
createCacheClusterResponse_httpStatus :: Lens.Lens' CreateCacheClusterResponse Prelude.Int
createCacheClusterResponse_httpStatus = Lens.lens (\CreateCacheClusterResponse' {httpStatus} -> httpStatus) (\s@CreateCacheClusterResponse' {} a -> s {httpStatus = a} :: CreateCacheClusterResponse)

instance Prelude.NFData CreateCacheClusterResponse
