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
-- Module      : Amazonka.ElastiCache.CreateReplicationGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Redis (cluster mode disabled) or a Redis (cluster mode
-- enabled) replication group.
--
-- This API can be used to create a standalone regional replication group
-- or a secondary replication group associated with a Global datastore.
--
-- A Redis (cluster mode disabled) replication group is a collection of
-- clusters, where one of the clusters is a read\/write primary and the
-- others are read-only replicas. Writes to the primary are asynchronously
-- propagated to the replicas.
--
-- A Redis cluster-mode enabled cluster is comprised of from 1 to 90 shards
-- (API\/CLI: node groups). Each shard has a primary node and up to 5
-- read-only replica nodes. The configuration can range from 90 shards and
-- 0 replicas to 15 shards and 5 replicas, which is the maximum number or
-- replicas allowed.
--
-- The node or shard limit can be increased to a maximum of 500 per cluster
-- if the Redis engine version is 5.0.6 or higher. For example, you can
-- choose to configure a 500 node cluster that ranges between 83 shards
-- (one primary and 5 replicas per shard) and 500 shards (single primary
-- and no replicas). Make sure there are enough available IP addresses to
-- accommodate the increase. Common pitfalls include the subnets in the
-- subnet group have too small a CIDR range or the subnets are shared and
-- heavily used by other clusters. For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.Creating.html Creating a Subnet Group>.
-- For versions below 5.0.6, the limit is 250 per cluster.
--
-- To request a limit increase, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html Amazon Service Limits>
-- and choose the limit type __Nodes per cluster per instance type__.
--
-- When a Redis (cluster mode disabled) replication group has been
-- successfully created, you can add one or more read replicas to it, up to
-- a total of 5 read replicas. If you need to increase or decrease the
-- number of node groups (console: shards), you can avail yourself of
-- ElastiCache for Redis\' scaling. For more information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Scaling.html Scaling ElastiCache for Redis Clusters>
-- in the /ElastiCache User Guide/.
--
-- This operation is valid for Redis only.
module Amazonka.ElastiCache.CreateReplicationGroup
  ( -- * Creating a Request
    CreateReplicationGroup (..),
    newCreateReplicationGroup,

    -- * Request Lenses
    createReplicationGroup_automaticFailoverEnabled,
    createReplicationGroup_engineVersion,
    createReplicationGroup_cacheNodeType,
    createReplicationGroup_nodeGroupConfiguration,
    createReplicationGroup_atRestEncryptionEnabled,
    createReplicationGroup_securityGroupIds,
    createReplicationGroup_snapshotArns,
    createReplicationGroup_autoMinorVersionUpgrade,
    createReplicationGroup_cacheParameterGroupName,
    createReplicationGroup_transitEncryptionEnabled,
    createReplicationGroup_userGroupIds,
    createReplicationGroup_snapshotWindow,
    createReplicationGroup_logDeliveryConfigurations,
    createReplicationGroup_authToken,
    createReplicationGroup_primaryClusterId,
    createReplicationGroup_engine,
    createReplicationGroup_preferredMaintenanceWindow,
    createReplicationGroup_kmsKeyId,
    createReplicationGroup_multiAZEnabled,
    createReplicationGroup_cacheSubnetGroupName,
    createReplicationGroup_numNodeGroups,
    createReplicationGroup_snapshotRetentionLimit,
    createReplicationGroup_globalReplicationGroupId,
    createReplicationGroup_replicasPerNodeGroup,
    createReplicationGroup_numCacheClusters,
    createReplicationGroup_preferredCacheClusterAZs,
    createReplicationGroup_snapshotName,
    createReplicationGroup_notificationTopicArn,
    createReplicationGroup_tags,
    createReplicationGroup_port,
    createReplicationGroup_cacheSecurityGroupNames,
    createReplicationGroup_replicationGroupId,
    createReplicationGroup_replicationGroupDescription,

    -- * Destructuring the Response
    CreateReplicationGroupResponse (..),
    newCreateReplicationGroupResponse,

    -- * Response Lenses
    createReplicationGroupResponse_replicationGroup,
    createReplicationGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateReplicationGroup@ operation.
--
-- /See:/ 'newCreateReplicationGroup' smart constructor.
data CreateReplicationGroup = CreateReplicationGroup'
  { -- | Specifies whether a read-only replica is automatically promoted to
    -- read\/write primary if the existing primary fails.
    --
    -- @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode
    -- enabled) replication groups.
    --
    -- Default: false
    automaticFailoverEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the cache engine to be used for the clusters in
    -- this replication group. To view the supported cache engine versions, use
    -- the @DescribeCacheEngineVersions@ operation.
    --
    -- __Important:__ You can upgrade to a newer engine version (see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>)
    -- in the /ElastiCache User Guide/, but you cannot downgrade to an earlier
    -- engine version. If you want to use an earlier engine version, you must
    -- delete the existing cluster or replication group and create it anew with
    -- the earlier engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
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
    -- | A list of node group (shard) configuration options. Each node group
    -- (shard) configuration has the following members:
    -- @PrimaryAvailabilityZone@, @ReplicaAvailabilityZones@, @ReplicaCount@,
    -- and @Slots@.
    --
    -- If you\'re creating a Redis (cluster mode disabled) or a Redis (cluster
    -- mode enabled) replication group, you can use this parameter to
    -- individually configure each node group (shard), or you can omit this
    -- parameter. However, it is required when seeding a Redis (cluster mode
    -- enabled) cluster from a S3 rdb file. You must configure each node group
    -- (shard) using this parameter because you must specify the slots for each
    -- node group.
    nodeGroupConfiguration :: Prelude.Maybe [NodeGroupConfiguration],
    -- | A flag that enables encryption at rest when set to @true@.
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the
    -- replication group is created. To enable encryption at rest on a
    -- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
    -- you create the replication group.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    atRestEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | One or more Amazon VPC security groups associated with this replication
    -- group.
    --
    -- Use this parameter only when you are creating a replication group in an
    -- Amazon Virtual Private Cloud (Amazon VPC).
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of Amazon Resource Names (ARN) that uniquely identify the Redis
    -- RDB snapshot files stored in Amazon S3. The snapshot files are used to
    -- populate the new replication group. The Amazon S3 object name in the ARN
    -- cannot contain any commas. The new replication group will have the
    -- number of node groups (console: shards) specified by the parameter
    -- /NumNodeGroups/ or the number of node groups configured by
    -- /NodeGroupConfiguration/ regardless of the number of ARNs specified
    -- here.
    --
    -- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
    snapshotArns :: Prelude.Maybe [Prelude.Text],
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The name of the parameter group to associate with this replication
    -- group. If this argument is omitted, the default cache parameter group
    -- for the specified engine is used.
    --
    -- If you are running Redis version 3.2.4 or later, only one node group
    -- (shard), and want to use a default parameter group, we recommend that
    -- you specify the parameter group by name.
    --
    -- -   To create a Redis (cluster mode disabled) replication group, use
    --     @CacheParameterGroupName=default.redis3.2@.
    --
    -- -   To create a Redis (cluster mode enabled) replication group, use
    --     @CacheParameterGroupName=default.redis3.2.cluster.on@.
    cacheParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables in-transit encryption when set to @true@.
    --
    -- You cannot modify the value of @TransitEncryptionEnabled@ after the
    -- cluster is created. To enable in-transit encryption on a cluster you
    -- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
    --
    -- This parameter is valid only if the @Engine@ parameter is @redis@, the
    -- @EngineVersion@ parameter is @3.2.6@, @4.x@ or later, and the cluster is
    -- being created in an Amazon VPC.
    --
    -- If you enable in-transit encryption, you must also specify a value for
    -- @CacheSubnetGroup@.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    --
    -- For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as
    -- @true@, an @AuthToken@, and a @CacheSubnetGroup@.
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The user group to associate with the replication group.
    userGroupIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of your node group (shard).
    --
    -- Example: @05:00-09:00@
    --
    -- If you do not specify this parameter, ElastiCache automatically chooses
    -- an appropriate time range.
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the destination, format and type of the logs.
    logDeliveryConfigurations :: Prelude.Maybe [LogDeliveryConfigurationRequest],
    -- | __Reserved parameter.__ The password used to access a password protected
    -- server.
    --
    -- @AuthToken@ can be specified only on replication groups where
    -- @TransitEncryptionEnabled@ is @true@.
    --
    -- For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as
    -- @true@, an @AuthToken@, and a @CacheSubnetGroup@.
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
    -- | The identifier of the cluster that serves as the primary for this
    -- replication group. This cluster must already exist and have a status of
    -- @available@.
    --
    -- This parameter is not required if @NumCacheClusters@, @NumNodeGroups@,
    -- or @ReplicasPerNodeGroup@ is specified.
    primaryClusterId :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache engine to be used for the clusters in this
    -- replication group. Must be Redis.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period. Valid values for @ddd@ are:
    --
    -- Specifies the weekly time range during which maintenance on the cluster
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
    -- | The ID of the KMS key used to encrypt the disk in the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A flag indicating if you have Multi-AZ enabled to enhance fault
    -- tolerance. For more information, see
    -- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>.
    multiAZEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the cache subnet group to be used for the replication group.
    --
    -- If you\'re going to launch your cluster in an Amazon VPC, you need to
    -- create a subnet group before you start creating a cluster. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups>.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the number of node groups (shards)
    -- for this Redis (cluster mode enabled) replication group. For Redis
    -- (cluster mode disabled) either omit this parameter or set it to 1.
    --
    -- Default: 1
    numNodeGroups :: Prelude.Maybe Prelude.Int,
    -- | The number of days for which ElastiCache retains automatic snapshots
    -- before deleting them. For example, if you set @SnapshotRetentionLimit@
    -- to 5, a snapshot that was taken today is retained for 5 days before
    -- being deleted.
    --
    -- Default: 0 (i.e., automatic backups are disabled for this cluster).
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The name of the Global datastore
    globalReplicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the number of replica nodes in each
    -- node group (shard). Valid values are 0 to 5.
    replicasPerNodeGroup :: Prelude.Maybe Prelude.Int,
    -- | The number of clusters this replication group initially has.
    --
    -- This parameter is not used if there is more than one node group (shard).
    -- You should use @ReplicasPerNodeGroup@ instead.
    --
    -- If @AutomaticFailoverEnabled@ is @true@, the value of this parameter
    -- must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can
    -- omit this parameter (it will default to 1), or you can explicitly set it
    -- to a value between 2 and 6.
    --
    -- The maximum permitted value for @NumCacheClusters@ is 6 (1 primary plus
    -- 5 replicas).
    numCacheClusters :: Prelude.Maybe Prelude.Int,
    -- | A list of EC2 Availability Zones in which the replication group\'s
    -- clusters are created. The order of the Availability Zones in the list is
    -- the order in which clusters are allocated. The primary cluster is
    -- created in the first AZ in the list.
    --
    -- This parameter is not used if there is more than one node group (shard).
    -- You should use @NodeGroupConfiguration@ instead.
    --
    -- If you are creating your replication group in an Amazon VPC
    -- (recommended), you can only locate clusters in Availability Zones
    -- associated with the subnets in the selected subnet group.
    --
    -- The number of Availability Zones listed must equal the value of
    -- @NumCacheClusters@.
    --
    -- Default: system chosen Availability Zones.
    preferredCacheClusterAZs :: Prelude.Maybe [Prelude.Text],
    -- | The name of a snapshot from which to restore data into the new
    -- replication group. The snapshot status changes to @restoring@ while the
    -- new replication group is being created.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- (SNS) topic to which notifications are sent.
    --
    -- The Amazon SNS topic owner must be the same as the cluster owner.
    notificationTopicArn :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to be added to this resource. Tags are comma-separated
    -- key,value pairs (e.g. Key=@myKey@, Value=@myKeyValue@. You can include
    -- multiple tags as shown following: Key=@myKey@, Value=@myKeyValue@
    -- Key=@mySecondKey@, Value=@mySecondKeyValue@. Tags on replication groups
    -- will be replicated to all nodes.
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which each member of the replication group accepts
    -- connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | A list of cache security group names to associate with this replication
    -- group.
    cacheSecurityGroupNames :: Prelude.Maybe [Prelude.Text],
    -- | The replication group identifier. This parameter is stored as a
    -- lowercase string.
    --
    -- Constraints:
    --
    -- -   A name must contain from 1 to 40 alphanumeric characters or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   A name cannot end with a hyphen or contain two consecutive hyphens.
    replicationGroupId :: Prelude.Text,
    -- | A user-created description for the replication group.
    replicationGroupDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticFailoverEnabled', 'createReplicationGroup_automaticFailoverEnabled' - Specifies whether a read-only replica is automatically promoted to
-- read\/write primary if the existing primary fails.
--
-- @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode
-- enabled) replication groups.
--
-- Default: false
--
-- 'engineVersion', 'createReplicationGroup_engineVersion' - The version number of the cache engine to be used for the clusters in
-- this replication group. To view the supported cache engine versions, use
-- the @DescribeCacheEngineVersions@ operation.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>)
-- in the /ElastiCache User Guide/, but you cannot downgrade to an earlier
-- engine version. If you want to use an earlier engine version, you must
-- delete the existing cluster or replication group and create it anew with
-- the earlier engine version.
--
-- 'cacheNodeType', 'createReplicationGroup_cacheNodeType' - The compute and memory capacity of the nodes in the node group (shard).
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
-- 'nodeGroupConfiguration', 'createReplicationGroup_nodeGroupConfiguration' - A list of node group (shard) configuration options. Each node group
-- (shard) configuration has the following members:
-- @PrimaryAvailabilityZone@, @ReplicaAvailabilityZones@, @ReplicaCount@,
-- and @Slots@.
--
-- If you\'re creating a Redis (cluster mode disabled) or a Redis (cluster
-- mode enabled) replication group, you can use this parameter to
-- individually configure each node group (shard), or you can omit this
-- parameter. However, it is required when seeding a Redis (cluster mode
-- enabled) cluster from a S3 rdb file. You must configure each node group
-- (shard) using this parameter because you must specify the slots for each
-- node group.
--
-- 'atRestEncryptionEnabled', 'createReplicationGroup_atRestEncryptionEnabled' - A flag that enables encryption at rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- replication group is created. To enable encryption at rest on a
-- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
-- you create the replication group.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- 'securityGroupIds', 'createReplicationGroup_securityGroupIds' - One or more Amazon VPC security groups associated with this replication
-- group.
--
-- Use this parameter only when you are creating a replication group in an
-- Amazon Virtual Private Cloud (Amazon VPC).
--
-- 'snapshotArns', 'createReplicationGroup_snapshotArns' - A list of Amazon Resource Names (ARN) that uniquely identify the Redis
-- RDB snapshot files stored in Amazon S3. The snapshot files are used to
-- populate the new replication group. The Amazon S3 object name in the ARN
-- cannot contain any commas. The new replication group will have the
-- number of node groups (console: shards) specified by the parameter
-- /NumNodeGroups/ or the number of node groups configured by
-- /NodeGroupConfiguration/ regardless of the number of ARNs specified
-- here.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
--
-- 'autoMinorVersionUpgrade', 'createReplicationGroup_autoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- 'cacheParameterGroupName', 'createReplicationGroup_cacheParameterGroupName' - The name of the parameter group to associate with this replication
-- group. If this argument is omitted, the default cache parameter group
-- for the specified engine is used.
--
-- If you are running Redis version 3.2.4 or later, only one node group
-- (shard), and want to use a default parameter group, we recommend that
-- you specify the parameter group by name.
--
-- -   To create a Redis (cluster mode disabled) replication group, use
--     @CacheParameterGroupName=default.redis3.2@.
--
-- -   To create a Redis (cluster mode enabled) replication group, use
--     @CacheParameterGroupName=default.redis3.2.cluster.on@.
--
-- 'transitEncryptionEnabled', 'createReplicationGroup_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@.
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the
-- cluster is created. To enable in-transit encryption on a cluster you
-- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
--
-- This parameter is valid only if the @Engine@ parameter is @redis@, the
-- @EngineVersion@ parameter is @3.2.6@, @4.x@ or later, and the cluster is
-- being created in an Amazon VPC.
--
-- If you enable in-transit encryption, you must also specify a value for
-- @CacheSubnetGroup@.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as
-- @true@, an @AuthToken@, and a @CacheSubnetGroup@.
--
-- 'userGroupIds', 'createReplicationGroup_userGroupIds' - The user group to associate with the replication group.
--
-- 'snapshotWindow', 'createReplicationGroup_snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
--
-- 'logDeliveryConfigurations', 'createReplicationGroup_logDeliveryConfigurations' - Specifies the destination, format and type of the logs.
--
-- 'authToken', 'createReplicationGroup_authToken' - __Reserved parameter.__ The password used to access a password protected
-- server.
--
-- @AuthToken@ can be specified only on replication groups where
-- @TransitEncryptionEnabled@ is @true@.
--
-- For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as
-- @true@, an @AuthToken@, and a @CacheSubnetGroup@.
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
-- 'primaryClusterId', 'createReplicationGroup_primaryClusterId' - The identifier of the cluster that serves as the primary for this
-- replication group. This cluster must already exist and have a status of
-- @available@.
--
-- This parameter is not required if @NumCacheClusters@, @NumNodeGroups@,
-- or @ReplicasPerNodeGroup@ is specified.
--
-- 'engine', 'createReplicationGroup_engine' - The name of the cache engine to be used for the clusters in this
-- replication group. Must be Redis.
--
-- 'preferredMaintenanceWindow', 'createReplicationGroup_preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for @ddd@ are:
--
-- Specifies the weekly time range during which maintenance on the cluster
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
-- 'kmsKeyId', 'createReplicationGroup_kmsKeyId' - The ID of the KMS key used to encrypt the disk in the cluster.
--
-- 'multiAZEnabled', 'createReplicationGroup_multiAZEnabled' - A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>.
--
-- 'cacheSubnetGroupName', 'createReplicationGroup_cacheSubnetGroupName' - The name of the cache subnet group to be used for the replication group.
--
-- If you\'re going to launch your cluster in an Amazon VPC, you need to
-- create a subnet group before you start creating a cluster. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups>.
--
-- 'numNodeGroups', 'createReplicationGroup_numNodeGroups' - An optional parameter that specifies the number of node groups (shards)
-- for this Redis (cluster mode enabled) replication group. For Redis
-- (cluster mode disabled) either omit this parameter or set it to 1.
--
-- Default: 1
--
-- 'snapshotRetentionLimit', 'createReplicationGroup_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic snapshots
-- before deleting them. For example, if you set @SnapshotRetentionLimit@
-- to 5, a snapshot that was taken today is retained for 5 days before
-- being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cluster).
--
-- 'globalReplicationGroupId', 'createReplicationGroup_globalReplicationGroupId' - The name of the Global datastore
--
-- 'replicasPerNodeGroup', 'createReplicationGroup_replicasPerNodeGroup' - An optional parameter that specifies the number of replica nodes in each
-- node group (shard). Valid values are 0 to 5.
--
-- 'numCacheClusters', 'createReplicationGroup_numCacheClusters' - The number of clusters this replication group initially has.
--
-- This parameter is not used if there is more than one node group (shard).
-- You should use @ReplicasPerNodeGroup@ instead.
--
-- If @AutomaticFailoverEnabled@ is @true@, the value of this parameter
-- must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can
-- omit this parameter (it will default to 1), or you can explicitly set it
-- to a value between 2 and 6.
--
-- The maximum permitted value for @NumCacheClusters@ is 6 (1 primary plus
-- 5 replicas).
--
-- 'preferredCacheClusterAZs', 'createReplicationGroup_preferredCacheClusterAZs' - A list of EC2 Availability Zones in which the replication group\'s
-- clusters are created. The order of the Availability Zones in the list is
-- the order in which clusters are allocated. The primary cluster is
-- created in the first AZ in the list.
--
-- This parameter is not used if there is more than one node group (shard).
-- You should use @NodeGroupConfiguration@ instead.
--
-- If you are creating your replication group in an Amazon VPC
-- (recommended), you can only locate clusters in Availability Zones
-- associated with the subnets in the selected subnet group.
--
-- The number of Availability Zones listed must equal the value of
-- @NumCacheClusters@.
--
-- Default: system chosen Availability Zones.
--
-- 'snapshotName', 'createReplicationGroup_snapshotName' - The name of a snapshot from which to restore data into the new
-- replication group. The snapshot status changes to @restoring@ while the
-- new replication group is being created.
--
-- 'notificationTopicArn', 'createReplicationGroup_notificationTopicArn' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications are sent.
--
-- The Amazon SNS topic owner must be the same as the cluster owner.
--
-- 'tags', 'createReplicationGroup_tags' - A list of tags to be added to this resource. Tags are comma-separated
-- key,value pairs (e.g. Key=@myKey@, Value=@myKeyValue@. You can include
-- multiple tags as shown following: Key=@myKey@, Value=@myKeyValue@
-- Key=@mySecondKey@, Value=@mySecondKeyValue@. Tags on replication groups
-- will be replicated to all nodes.
--
-- 'port', 'createReplicationGroup_port' - The port number on which each member of the replication group accepts
-- connections.
--
-- 'cacheSecurityGroupNames', 'createReplicationGroup_cacheSecurityGroupNames' - A list of cache security group names to associate with this replication
-- group.
--
-- 'replicationGroupId', 'createReplicationGroup_replicationGroupId' - The replication group identifier. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   A name must contain from 1 to 40 alphanumeric characters or hyphens.
--
-- -   The first character must be a letter.
--
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
--
-- 'replicationGroupDescription', 'createReplicationGroup_replicationGroupDescription' - A user-created description for the replication group.
newCreateReplicationGroup ::
  -- | 'replicationGroupId'
  Prelude.Text ->
  -- | 'replicationGroupDescription'
  Prelude.Text ->
  CreateReplicationGroup
newCreateReplicationGroup
  pReplicationGroupId_
  pReplicationGroupDescription_ =
    CreateReplicationGroup'
      { automaticFailoverEnabled =
          Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        cacheNodeType = Prelude.Nothing,
        nodeGroupConfiguration = Prelude.Nothing,
        atRestEncryptionEnabled = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        snapshotArns = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        cacheParameterGroupName = Prelude.Nothing,
        transitEncryptionEnabled = Prelude.Nothing,
        userGroupIds = Prelude.Nothing,
        snapshotWindow = Prelude.Nothing,
        logDeliveryConfigurations = Prelude.Nothing,
        authToken = Prelude.Nothing,
        primaryClusterId = Prelude.Nothing,
        engine = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        multiAZEnabled = Prelude.Nothing,
        cacheSubnetGroupName = Prelude.Nothing,
        numNodeGroups = Prelude.Nothing,
        snapshotRetentionLimit = Prelude.Nothing,
        globalReplicationGroupId = Prelude.Nothing,
        replicasPerNodeGroup = Prelude.Nothing,
        numCacheClusters = Prelude.Nothing,
        preferredCacheClusterAZs = Prelude.Nothing,
        snapshotName = Prelude.Nothing,
        notificationTopicArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        cacheSecurityGroupNames = Prelude.Nothing,
        replicationGroupId = pReplicationGroupId_,
        replicationGroupDescription =
          pReplicationGroupDescription_
      }

-- | Specifies whether a read-only replica is automatically promoted to
-- read\/write primary if the existing primary fails.
--
-- @AutomaticFailoverEnabled@ must be enabled for Redis (cluster mode
-- enabled) replication groups.
--
-- Default: false
createReplicationGroup_automaticFailoverEnabled :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Bool)
createReplicationGroup_automaticFailoverEnabled = Lens.lens (\CreateReplicationGroup' {automaticFailoverEnabled} -> automaticFailoverEnabled) (\s@CreateReplicationGroup' {} a -> s {automaticFailoverEnabled = a} :: CreateReplicationGroup)

-- | The version number of the cache engine to be used for the clusters in
-- this replication group. To view the supported cache engine versions, use
-- the @DescribeCacheEngineVersions@ operation.
--
-- __Important:__ You can upgrade to a newer engine version (see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>)
-- in the /ElastiCache User Guide/, but you cannot downgrade to an earlier
-- engine version. If you want to use an earlier engine version, you must
-- delete the existing cluster or replication group and create it anew with
-- the earlier engine version.
createReplicationGroup_engineVersion :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_engineVersion = Lens.lens (\CreateReplicationGroup' {engineVersion} -> engineVersion) (\s@CreateReplicationGroup' {} a -> s {engineVersion = a} :: CreateReplicationGroup)

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
createReplicationGroup_cacheNodeType :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_cacheNodeType = Lens.lens (\CreateReplicationGroup' {cacheNodeType} -> cacheNodeType) (\s@CreateReplicationGroup' {} a -> s {cacheNodeType = a} :: CreateReplicationGroup)

-- | A list of node group (shard) configuration options. Each node group
-- (shard) configuration has the following members:
-- @PrimaryAvailabilityZone@, @ReplicaAvailabilityZones@, @ReplicaCount@,
-- and @Slots@.
--
-- If you\'re creating a Redis (cluster mode disabled) or a Redis (cluster
-- mode enabled) replication group, you can use this parameter to
-- individually configure each node group (shard), or you can omit this
-- parameter. However, it is required when seeding a Redis (cluster mode
-- enabled) cluster from a S3 rdb file. You must configure each node group
-- (shard) using this parameter because you must specify the slots for each
-- node group.
createReplicationGroup_nodeGroupConfiguration :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe [NodeGroupConfiguration])
createReplicationGroup_nodeGroupConfiguration = Lens.lens (\CreateReplicationGroup' {nodeGroupConfiguration} -> nodeGroupConfiguration) (\s@CreateReplicationGroup' {} a -> s {nodeGroupConfiguration = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | A flag that enables encryption at rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- replication group is created. To enable encryption at rest on a
-- replication group you must set @AtRestEncryptionEnabled@ to @true@ when
-- you create the replication group.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
createReplicationGroup_atRestEncryptionEnabled :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Bool)
createReplicationGroup_atRestEncryptionEnabled = Lens.lens (\CreateReplicationGroup' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@CreateReplicationGroup' {} a -> s {atRestEncryptionEnabled = a} :: CreateReplicationGroup)

-- | One or more Amazon VPC security groups associated with this replication
-- group.
--
-- Use this parameter only when you are creating a replication group in an
-- Amazon Virtual Private Cloud (Amazon VPC).
createReplicationGroup_securityGroupIds :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe [Prelude.Text])
createReplicationGroup_securityGroupIds = Lens.lens (\CreateReplicationGroup' {securityGroupIds} -> securityGroupIds) (\s@CreateReplicationGroup' {} a -> s {securityGroupIds = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | A list of Amazon Resource Names (ARN) that uniquely identify the Redis
-- RDB snapshot files stored in Amazon S3. The snapshot files are used to
-- populate the new replication group. The Amazon S3 object name in the ARN
-- cannot contain any commas. The new replication group will have the
-- number of node groups (console: shards) specified by the parameter
-- /NumNodeGroups/ or the number of node groups configured by
-- /NodeGroupConfiguration/ regardless of the number of ARNs specified
-- here.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
createReplicationGroup_snapshotArns :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe [Prelude.Text])
createReplicationGroup_snapshotArns = Lens.lens (\CreateReplicationGroup' {snapshotArns} -> snapshotArns) (\s@CreateReplicationGroup' {} a -> s {snapshotArns = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | This parameter is currently disabled.
createReplicationGroup_autoMinorVersionUpgrade :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Bool)
createReplicationGroup_autoMinorVersionUpgrade = Lens.lens (\CreateReplicationGroup' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateReplicationGroup' {} a -> s {autoMinorVersionUpgrade = a} :: CreateReplicationGroup)

-- | The name of the parameter group to associate with this replication
-- group. If this argument is omitted, the default cache parameter group
-- for the specified engine is used.
--
-- If you are running Redis version 3.2.4 or later, only one node group
-- (shard), and want to use a default parameter group, we recommend that
-- you specify the parameter group by name.
--
-- -   To create a Redis (cluster mode disabled) replication group, use
--     @CacheParameterGroupName=default.redis3.2@.
--
-- -   To create a Redis (cluster mode enabled) replication group, use
--     @CacheParameterGroupName=default.redis3.2.cluster.on@.
createReplicationGroup_cacheParameterGroupName :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_cacheParameterGroupName = Lens.lens (\CreateReplicationGroup' {cacheParameterGroupName} -> cacheParameterGroupName) (\s@CreateReplicationGroup' {} a -> s {cacheParameterGroupName = a} :: CreateReplicationGroup)

-- | A flag that enables in-transit encryption when set to @true@.
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the
-- cluster is created. To enable in-transit encryption on a cluster you
-- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
--
-- This parameter is valid only if the @Engine@ parameter is @redis@, the
-- @EngineVersion@ parameter is @3.2.6@, @4.x@ or later, and the cluster is
-- being created in an Amazon VPC.
--
-- If you enable in-transit encryption, you must also specify a value for
-- @CacheSubnetGroup@.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as
-- @true@, an @AuthToken@, and a @CacheSubnetGroup@.
createReplicationGroup_transitEncryptionEnabled :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Bool)
createReplicationGroup_transitEncryptionEnabled = Lens.lens (\CreateReplicationGroup' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@CreateReplicationGroup' {} a -> s {transitEncryptionEnabled = a} :: CreateReplicationGroup)

-- | The user group to associate with the replication group.
createReplicationGroup_userGroupIds :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createReplicationGroup_userGroupIds = Lens.lens (\CreateReplicationGroup' {userGroupIds} -> userGroupIds) (\s@CreateReplicationGroup' {} a -> s {userGroupIds = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your node group (shard).
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, ElastiCache automatically chooses
-- an appropriate time range.
createReplicationGroup_snapshotWindow :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_snapshotWindow = Lens.lens (\CreateReplicationGroup' {snapshotWindow} -> snapshotWindow) (\s@CreateReplicationGroup' {} a -> s {snapshotWindow = a} :: CreateReplicationGroup)

-- | Specifies the destination, format and type of the logs.
createReplicationGroup_logDeliveryConfigurations :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe [LogDeliveryConfigurationRequest])
createReplicationGroup_logDeliveryConfigurations = Lens.lens (\CreateReplicationGroup' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@CreateReplicationGroup' {} a -> s {logDeliveryConfigurations = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | __Reserved parameter.__ The password used to access a password protected
-- server.
--
-- @AuthToken@ can be specified only on replication groups where
-- @TransitEncryptionEnabled@ is @true@.
--
-- For HIPAA compliance, you must specify @TransitEncryptionEnabled@ as
-- @true@, an @AuthToken@, and a @CacheSubnetGroup@.
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
createReplicationGroup_authToken :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_authToken = Lens.lens (\CreateReplicationGroup' {authToken} -> authToken) (\s@CreateReplicationGroup' {} a -> s {authToken = a} :: CreateReplicationGroup)

-- | The identifier of the cluster that serves as the primary for this
-- replication group. This cluster must already exist and have a status of
-- @available@.
--
-- This parameter is not required if @NumCacheClusters@, @NumNodeGroups@,
-- or @ReplicasPerNodeGroup@ is specified.
createReplicationGroup_primaryClusterId :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_primaryClusterId = Lens.lens (\CreateReplicationGroup' {primaryClusterId} -> primaryClusterId) (\s@CreateReplicationGroup' {} a -> s {primaryClusterId = a} :: CreateReplicationGroup)

-- | The name of the cache engine to be used for the clusters in this
-- replication group. Must be Redis.
createReplicationGroup_engine :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_engine = Lens.lens (\CreateReplicationGroup' {engine} -> engine) (\s@CreateReplicationGroup' {} a -> s {engine = a} :: CreateReplicationGroup)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for @ddd@ are:
--
-- Specifies the weekly time range during which maintenance on the cluster
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
createReplicationGroup_preferredMaintenanceWindow :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_preferredMaintenanceWindow = Lens.lens (\CreateReplicationGroup' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateReplicationGroup' {} a -> s {preferredMaintenanceWindow = a} :: CreateReplicationGroup)

-- | The ID of the KMS key used to encrypt the disk in the cluster.
createReplicationGroup_kmsKeyId :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_kmsKeyId = Lens.lens (\CreateReplicationGroup' {kmsKeyId} -> kmsKeyId) (\s@CreateReplicationGroup' {} a -> s {kmsKeyId = a} :: CreateReplicationGroup)

-- | A flag indicating if you have Multi-AZ enabled to enhance fault
-- tolerance. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/AutoFailover.html Minimizing Downtime: Multi-AZ>.
createReplicationGroup_multiAZEnabled :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Bool)
createReplicationGroup_multiAZEnabled = Lens.lens (\CreateReplicationGroup' {multiAZEnabled} -> multiAZEnabled) (\s@CreateReplicationGroup' {} a -> s {multiAZEnabled = a} :: CreateReplicationGroup)

-- | The name of the cache subnet group to be used for the replication group.
--
-- If you\'re going to launch your cluster in an Amazon VPC, you need to
-- create a subnet group before you start creating a cluster. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/SubnetGroups.html Subnets and Subnet Groups>.
createReplicationGroup_cacheSubnetGroupName :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_cacheSubnetGroupName = Lens.lens (\CreateReplicationGroup' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@CreateReplicationGroup' {} a -> s {cacheSubnetGroupName = a} :: CreateReplicationGroup)

-- | An optional parameter that specifies the number of node groups (shards)
-- for this Redis (cluster mode enabled) replication group. For Redis
-- (cluster mode disabled) either omit this parameter or set it to 1.
--
-- Default: 1
createReplicationGroup_numNodeGroups :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Int)
createReplicationGroup_numNodeGroups = Lens.lens (\CreateReplicationGroup' {numNodeGroups} -> numNodeGroups) (\s@CreateReplicationGroup' {} a -> s {numNodeGroups = a} :: CreateReplicationGroup)

-- | The number of days for which ElastiCache retains automatic snapshots
-- before deleting them. For example, if you set @SnapshotRetentionLimit@
-- to 5, a snapshot that was taken today is retained for 5 days before
-- being deleted.
--
-- Default: 0 (i.e., automatic backups are disabled for this cluster).
createReplicationGroup_snapshotRetentionLimit :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Int)
createReplicationGroup_snapshotRetentionLimit = Lens.lens (\CreateReplicationGroup' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@CreateReplicationGroup' {} a -> s {snapshotRetentionLimit = a} :: CreateReplicationGroup)

-- | The name of the Global datastore
createReplicationGroup_globalReplicationGroupId :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_globalReplicationGroupId = Lens.lens (\CreateReplicationGroup' {globalReplicationGroupId} -> globalReplicationGroupId) (\s@CreateReplicationGroup' {} a -> s {globalReplicationGroupId = a} :: CreateReplicationGroup)

-- | An optional parameter that specifies the number of replica nodes in each
-- node group (shard). Valid values are 0 to 5.
createReplicationGroup_replicasPerNodeGroup :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Int)
createReplicationGroup_replicasPerNodeGroup = Lens.lens (\CreateReplicationGroup' {replicasPerNodeGroup} -> replicasPerNodeGroup) (\s@CreateReplicationGroup' {} a -> s {replicasPerNodeGroup = a} :: CreateReplicationGroup)

-- | The number of clusters this replication group initially has.
--
-- This parameter is not used if there is more than one node group (shard).
-- You should use @ReplicasPerNodeGroup@ instead.
--
-- If @AutomaticFailoverEnabled@ is @true@, the value of this parameter
-- must be at least 2. If @AutomaticFailoverEnabled@ is @false@ you can
-- omit this parameter (it will default to 1), or you can explicitly set it
-- to a value between 2 and 6.
--
-- The maximum permitted value for @NumCacheClusters@ is 6 (1 primary plus
-- 5 replicas).
createReplicationGroup_numCacheClusters :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Int)
createReplicationGroup_numCacheClusters = Lens.lens (\CreateReplicationGroup' {numCacheClusters} -> numCacheClusters) (\s@CreateReplicationGroup' {} a -> s {numCacheClusters = a} :: CreateReplicationGroup)

-- | A list of EC2 Availability Zones in which the replication group\'s
-- clusters are created. The order of the Availability Zones in the list is
-- the order in which clusters are allocated. The primary cluster is
-- created in the first AZ in the list.
--
-- This parameter is not used if there is more than one node group (shard).
-- You should use @NodeGroupConfiguration@ instead.
--
-- If you are creating your replication group in an Amazon VPC
-- (recommended), you can only locate clusters in Availability Zones
-- associated with the subnets in the selected subnet group.
--
-- The number of Availability Zones listed must equal the value of
-- @NumCacheClusters@.
--
-- Default: system chosen Availability Zones.
createReplicationGroup_preferredCacheClusterAZs :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe [Prelude.Text])
createReplicationGroup_preferredCacheClusterAZs = Lens.lens (\CreateReplicationGroup' {preferredCacheClusterAZs} -> preferredCacheClusterAZs) (\s@CreateReplicationGroup' {} a -> s {preferredCacheClusterAZs = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of a snapshot from which to restore data into the new
-- replication group. The snapshot status changes to @restoring@ while the
-- new replication group is being created.
createReplicationGroup_snapshotName :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_snapshotName = Lens.lens (\CreateReplicationGroup' {snapshotName} -> snapshotName) (\s@CreateReplicationGroup' {} a -> s {snapshotName = a} :: CreateReplicationGroup)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications are sent.
--
-- The Amazon SNS topic owner must be the same as the cluster owner.
createReplicationGroup_notificationTopicArn :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Text)
createReplicationGroup_notificationTopicArn = Lens.lens (\CreateReplicationGroup' {notificationTopicArn} -> notificationTopicArn) (\s@CreateReplicationGroup' {} a -> s {notificationTopicArn = a} :: CreateReplicationGroup)

-- | A list of tags to be added to this resource. Tags are comma-separated
-- key,value pairs (e.g. Key=@myKey@, Value=@myKeyValue@. You can include
-- multiple tags as shown following: Key=@myKey@, Value=@myKeyValue@
-- Key=@mySecondKey@, Value=@mySecondKeyValue@. Tags on replication groups
-- will be replicated to all nodes.
createReplicationGroup_tags :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe [Tag])
createReplicationGroup_tags = Lens.lens (\CreateReplicationGroup' {tags} -> tags) (\s@CreateReplicationGroup' {} a -> s {tags = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which each member of the replication group accepts
-- connections.
createReplicationGroup_port :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe Prelude.Int)
createReplicationGroup_port = Lens.lens (\CreateReplicationGroup' {port} -> port) (\s@CreateReplicationGroup' {} a -> s {port = a} :: CreateReplicationGroup)

-- | A list of cache security group names to associate with this replication
-- group.
createReplicationGroup_cacheSecurityGroupNames :: Lens.Lens' CreateReplicationGroup (Prelude.Maybe [Prelude.Text])
createReplicationGroup_cacheSecurityGroupNames = Lens.lens (\CreateReplicationGroup' {cacheSecurityGroupNames} -> cacheSecurityGroupNames) (\s@CreateReplicationGroup' {} a -> s {cacheSecurityGroupNames = a} :: CreateReplicationGroup) Prelude.. Lens.mapping Lens.coerced

-- | The replication group identifier. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   A name must contain from 1 to 40 alphanumeric characters or hyphens.
--
-- -   The first character must be a letter.
--
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
createReplicationGroup_replicationGroupId :: Lens.Lens' CreateReplicationGroup Prelude.Text
createReplicationGroup_replicationGroupId = Lens.lens (\CreateReplicationGroup' {replicationGroupId} -> replicationGroupId) (\s@CreateReplicationGroup' {} a -> s {replicationGroupId = a} :: CreateReplicationGroup)

-- | A user-created description for the replication group.
createReplicationGroup_replicationGroupDescription :: Lens.Lens' CreateReplicationGroup Prelude.Text
createReplicationGroup_replicationGroupDescription = Lens.lens (\CreateReplicationGroup' {replicationGroupDescription} -> replicationGroupDescription) (\s@CreateReplicationGroup' {} a -> s {replicationGroupDescription = a} :: CreateReplicationGroup)

instance Core.AWSRequest CreateReplicationGroup where
  type
    AWSResponse CreateReplicationGroup =
      CreateReplicationGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateReplicationGroupResult"
      ( \s h x ->
          CreateReplicationGroupResponse'
            Prelude.<$> (x Core..@? "ReplicationGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReplicationGroup where
  hashWithSalt _salt CreateReplicationGroup' {..} =
    _salt
      `Prelude.hashWithSalt` automaticFailoverEnabled
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` nodeGroupConfiguration
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` snapshotArns
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` cacheParameterGroupName
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` userGroupIds
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` authToken
      `Prelude.hashWithSalt` primaryClusterId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` multiAZEnabled
      `Prelude.hashWithSalt` cacheSubnetGroupName
      `Prelude.hashWithSalt` numNodeGroups
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` globalReplicationGroupId
      `Prelude.hashWithSalt` replicasPerNodeGroup
      `Prelude.hashWithSalt` numCacheClusters
      `Prelude.hashWithSalt` preferredCacheClusterAZs
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` notificationTopicArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` cacheSecurityGroupNames
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` replicationGroupDescription

instance Prelude.NFData CreateReplicationGroup where
  rnf CreateReplicationGroup' {..} =
    Prelude.rnf automaticFailoverEnabled
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf nodeGroupConfiguration
      `Prelude.seq` Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf snapshotArns
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf cacheParameterGroupName
      `Prelude.seq` Prelude.rnf transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf userGroupIds
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf authToken
      `Prelude.seq` Prelude.rnf primaryClusterId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf multiAZEnabled
      `Prelude.seq` Prelude.rnf
        cacheSubnetGroupName
      `Prelude.seq` Prelude.rnf numNodeGroups
      `Prelude.seq` Prelude.rnf
        snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf
        globalReplicationGroupId
      `Prelude.seq` Prelude.rnf
        replicasPerNodeGroup
      `Prelude.seq` Prelude.rnf
        numCacheClusters
      `Prelude.seq` Prelude.rnf
        preferredCacheClusterAZs
      `Prelude.seq` Prelude.rnf
        snapshotName
      `Prelude.seq` Prelude.rnf
        notificationTopicArn
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        port
      `Prelude.seq` Prelude.rnf
        cacheSecurityGroupNames
      `Prelude.seq` Prelude.rnf
        replicationGroupId
      `Prelude.seq` Prelude.rnf
        replicationGroupDescription

instance Core.ToHeaders CreateReplicationGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateReplicationGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateReplicationGroup where
  toQuery CreateReplicationGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateReplicationGroup" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "AutomaticFailoverEnabled"
          Core.=: automaticFailoverEnabled,
        "EngineVersion" Core.=: engineVersion,
        "CacheNodeType" Core.=: cacheNodeType,
        "NodeGroupConfiguration"
          Core.=: Core.toQuery
            ( Core.toQueryList "NodeGroupConfiguration"
                Prelude.<$> nodeGroupConfiguration
            ),
        "AtRestEncryptionEnabled"
          Core.=: atRestEncryptionEnabled,
        "SecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "SecurityGroupId"
                Prelude.<$> securityGroupIds
            ),
        "SnapshotArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "SnapshotArn"
                Prelude.<$> snapshotArns
            ),
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "CacheParameterGroupName"
          Core.=: cacheParameterGroupName,
        "TransitEncryptionEnabled"
          Core.=: transitEncryptionEnabled,
        "UserGroupIds"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> userGroupIds),
        "SnapshotWindow" Core.=: snapshotWindow,
        "LogDeliveryConfigurations"
          Core.=: Core.toQuery
            ( Core.toQueryList "LogDeliveryConfigurationRequest"
                Prelude.<$> logDeliveryConfigurations
            ),
        "AuthToken" Core.=: authToken,
        "PrimaryClusterId" Core.=: primaryClusterId,
        "Engine" Core.=: engine,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "KmsKeyId" Core.=: kmsKeyId,
        "MultiAZEnabled" Core.=: multiAZEnabled,
        "CacheSubnetGroupName" Core.=: cacheSubnetGroupName,
        "NumNodeGroups" Core.=: numNodeGroups,
        "SnapshotRetentionLimit"
          Core.=: snapshotRetentionLimit,
        "GlobalReplicationGroupId"
          Core.=: globalReplicationGroupId,
        "ReplicasPerNodeGroup" Core.=: replicasPerNodeGroup,
        "NumCacheClusters" Core.=: numCacheClusters,
        "PreferredCacheClusterAZs"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Prelude.<$> preferredCacheClusterAZs
            ),
        "SnapshotName" Core.=: snapshotName,
        "NotificationTopicArn" Core.=: notificationTopicArn,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "CacheSecurityGroupNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "CacheSecurityGroupName"
                Prelude.<$> cacheSecurityGroupNames
            ),
        "ReplicationGroupId" Core.=: replicationGroupId,
        "ReplicationGroupDescription"
          Core.=: replicationGroupDescription
      ]

-- | /See:/ 'newCreateReplicationGroupResponse' smart constructor.
data CreateReplicationGroupResponse = CreateReplicationGroupResponse'
  { replicationGroup :: Prelude.Maybe ReplicationGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationGroup', 'createReplicationGroupResponse_replicationGroup' - Undocumented member.
--
-- 'httpStatus', 'createReplicationGroupResponse_httpStatus' - The response's http status code.
newCreateReplicationGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReplicationGroupResponse
newCreateReplicationGroupResponse pHttpStatus_ =
  CreateReplicationGroupResponse'
    { replicationGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createReplicationGroupResponse_replicationGroup :: Lens.Lens' CreateReplicationGroupResponse (Prelude.Maybe ReplicationGroup)
createReplicationGroupResponse_replicationGroup = Lens.lens (\CreateReplicationGroupResponse' {replicationGroup} -> replicationGroup) (\s@CreateReplicationGroupResponse' {} a -> s {replicationGroup = a} :: CreateReplicationGroupResponse)

-- | The response's http status code.
createReplicationGroupResponse_httpStatus :: Lens.Lens' CreateReplicationGroupResponse Prelude.Int
createReplicationGroupResponse_httpStatus = Lens.lens (\CreateReplicationGroupResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationGroupResponse' {} a -> s {httpStatus = a} :: CreateReplicationGroupResponse)

instance
  Prelude.NFData
    CreateReplicationGroupResponse
  where
  rnf CreateReplicationGroupResponse' {..} =
    Prelude.rnf replicationGroup
      `Prelude.seq` Prelude.rnf httpStatus
