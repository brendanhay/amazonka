{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElastiCache.Types.CacheCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheCluster where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.CacheNode
import Amazonka.ElastiCache.Types.CacheParameterGroupStatus
import Amazonka.ElastiCache.Types.CacheSecurityGroupMembership
import Amazonka.ElastiCache.Types.Endpoint
import Amazonka.ElastiCache.Types.IpDiscovery
import Amazonka.ElastiCache.Types.LogDeliveryConfiguration
import Amazonka.ElastiCache.Types.NetworkType
import Amazonka.ElastiCache.Types.NotificationConfiguration
import Amazonka.ElastiCache.Types.PendingModifiedValues
import Amazonka.ElastiCache.Types.SecurityGroupMembership
import Amazonka.ElastiCache.Types.TransitEncryptionMode
import qualified Amazonka.Prelude as Prelude

-- | Contains all of the attributes of a specific cluster.
--
-- /See:/ 'newCacheCluster' smart constructor.
data CacheCluster = CacheCluster'
  { -- | The ARN (Amazon Resource Name) of the cache cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables encryption at-rest when set to @true@.
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the
    -- cluster is created. To enable at-rest encryption on a cluster you must
    -- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    atRestEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Prelude.Maybe Data.ISO8601,
    -- | If you are running Redis engine version 6.0 or later, set this
    -- parameter to yes if you want to opt-in to the next auto minor version
    -- upgrade campaign. This parameter is disabled for previous versions.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the cluster was created.
    cacheClusterCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The user-supplied identifier of the cluster. This identifier is a unique
    -- key that identifies a cluster.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The current state of this cluster, one of the following values:
    -- @available@, @creating@, @deleted@, @deleting@, @incompatible-network@,
    -- @modifying@, @rebooting cluster nodes@, @restore-failed@, or
    -- @snapshotting@.
    cacheClusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity node type for the cluster.
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
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward):
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
    --         __T4g node types__ (available only for Redis engine version
    --         5.0.6 onward and Memcached engine version 1.5.16 onward):
    --         @cache.t4g.micro@, @cache.t4g.small@, @cache.t4g.medium@
    --
    --         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
    --         @cache.t3.medium@
    --
    --         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
    --         @cache.t2.medium@
    --
    --     -   Previous generation: (not recommended. Existing clusters are
    --         still supported but creation of new clusters is not supported
    --         for these types.)
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
    --     -   Previous generation: (not recommended. Existing clusters are
    --         still supported but creation of new clusters is not supported
    --         for these types.)
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
    --     -   Previous generation: (not recommended. Existing clusters are
    --         still supported but creation of new clusters is not supported
    --         for these types.)
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
    -- | A list of cache nodes that are members of the cluster.
    cacheNodes :: Prelude.Maybe [CacheNode],
    -- | Status of the cache parameter group.
    cacheParameterGroup :: Prelude.Maybe CacheParameterGroupStatus,
    -- | A list of cache security group elements, composed of name and status
    -- sub-elements.
    cacheSecurityGroups :: Prelude.Maybe [CacheSecurityGroupMembership],
    -- | The name of the cache subnet group associated with the cluster.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The URL of the web page where you can download the latest ElastiCache
    -- client library.
    clientDownloadLandingPage :: Prelude.Maybe Prelude.Text,
    -- | Represents a Memcached cluster endpoint which can be used by an
    -- application to connect to any node in the cluster. The configuration
    -- endpoint will always have @.cfg@ in it.
    --
    -- Example: @mem-3.9dvc4r@/@.cfg@/@.usw2.cache.amazonaws.com:11211@
    configurationEndpoint :: Prelude.Maybe Endpoint,
    -- | The name of the cache engine (@memcached@ or @redis@) to be used for
    -- this cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | The version of the cache engine that is used in this cluster.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The network type associated with the cluster, either @ipv4@ | @ipv6@.
    -- IPv6 is supported for workloads using Redis engine version 6.2 onward or
    -- Memcached engine version 1.6.6 on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    ipDiscovery :: Prelude.Maybe IpDiscovery,
    -- | Returns the destination, format and type of the logs.
    logDeliveryConfigurations :: Prelude.Maybe [LogDeliveryConfiguration],
    -- | Must be either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for
    -- workloads using Redis engine version 6.2 onward or Memcached engine
    -- version 1.6.6 on all instances built on the
    -- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
    networkType :: Prelude.Maybe NetworkType,
    -- | Describes a notification topic and its status. Notification topics are
    -- used for publishing ElastiCache events to subscribers using Amazon
    -- Simple Notification Service (SNS).
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The number of cache nodes in the cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 40.
    numCacheNodes :: Prelude.Maybe Prelude.Int,
    pendingModifiedValues :: Prelude.Maybe PendingModifiedValues,
    -- | The name of the Availability Zone in which the cluster is located or
    -- \"Multiple\" if the cache nodes are located in different Availability
    -- Zones.
    preferredAvailabilityZone :: Prelude.Maybe Prelude.Text,
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
    -- | The outpost ARN in which the cache cluster is created.
    preferredOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | The replication group to which this cluster belongs. If this field is
    -- empty, the cluster is not associated with any replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    -- | A boolean value indicating whether log delivery is enabled for the
    -- replication group.
    replicationGroupLogDeliveryEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A list of VPC Security Groups associated with the cluster.
    securityGroups :: Prelude.Maybe [SecurityGroupMembership],
    -- | The number of days for which ElastiCache retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- If the value of SnapshotRetentionLimit is set to zero (0), backups are
    -- turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of your cluster.
    --
    -- Example: @05:00-09:00@
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables in-transit encryption when set to @true@.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A setting that allows you to migrate your clients to use in-transit
    -- encryption, with no downtime.
    transitEncryptionMode :: Prelude.Maybe TransitEncryptionMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'cacheCluster_arn' - The ARN (Amazon Resource Name) of the cache cluster.
--
-- 'atRestEncryptionEnabled', 'cacheCluster_atRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- cluster is created. To enable at-rest encryption on a cluster you must
-- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- 'authTokenEnabled', 'cacheCluster_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'authTokenLastModifiedDate', 'cacheCluster_authTokenLastModifiedDate' - The date the auth token was last modified
--
-- 'autoMinorVersionUpgrade', 'cacheCluster_autoMinorVersionUpgrade' - If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
--
-- 'cacheClusterCreateTime', 'cacheCluster_cacheClusterCreateTime' - The date and time when the cluster was created.
--
-- 'cacheClusterId', 'cacheCluster_cacheClusterId' - The user-supplied identifier of the cluster. This identifier is a unique
-- key that identifies a cluster.
--
-- 'cacheClusterStatus', 'cacheCluster_cacheClusterStatus' - The current state of this cluster, one of the following values:
-- @available@, @creating@, @deleted@, @deleting@, @incompatible-network@,
-- @modifying@, @rebooting cluster nodes@, @restore-failed@, or
-- @snapshotting@.
--
-- 'cacheNodeType', 'cacheCluster_cacheNodeType' - The name of the compute and memory capacity node type for the cluster.
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
--         5.0.6 onward and for Memcached engine version 1.5.16 onward):
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
--         __T4g node types__ (available only for Redis engine version
--         5.0.6 onward and Memcached engine version 1.5.16 onward):
--         @cache.t4g.micro@, @cache.t4g.small@, @cache.t4g.medium@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
-- 'cacheNodes', 'cacheCluster_cacheNodes' - A list of cache nodes that are members of the cluster.
--
-- 'cacheParameterGroup', 'cacheCluster_cacheParameterGroup' - Status of the cache parameter group.
--
-- 'cacheSecurityGroups', 'cacheCluster_cacheSecurityGroups' - A list of cache security group elements, composed of name and status
-- sub-elements.
--
-- 'cacheSubnetGroupName', 'cacheCluster_cacheSubnetGroupName' - The name of the cache subnet group associated with the cluster.
--
-- 'clientDownloadLandingPage', 'cacheCluster_clientDownloadLandingPage' - The URL of the web page where you can download the latest ElastiCache
-- client library.
--
-- 'configurationEndpoint', 'cacheCluster_configurationEndpoint' - Represents a Memcached cluster endpoint which can be used by an
-- application to connect to any node in the cluster. The configuration
-- endpoint will always have @.cfg@ in it.
--
-- Example: @mem-3.9dvc4r@/@.cfg@/@.usw2.cache.amazonaws.com:11211@
--
-- 'engine', 'cacheCluster_engine' - The name of the cache engine (@memcached@ or @redis@) to be used for
-- this cluster.
--
-- 'engineVersion', 'cacheCluster_engineVersion' - The version of the cache engine that is used in this cluster.
--
-- 'ipDiscovery', 'cacheCluster_ipDiscovery' - The network type associated with the cluster, either @ipv4@ | @ipv6@.
-- IPv6 is supported for workloads using Redis engine version 6.2 onward or
-- Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
--
-- 'logDeliveryConfigurations', 'cacheCluster_logDeliveryConfigurations' - Returns the destination, format and type of the logs.
--
-- 'networkType', 'cacheCluster_networkType' - Must be either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for
-- workloads using Redis engine version 6.2 onward or Memcached engine
-- version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
--
-- 'notificationConfiguration', 'cacheCluster_notificationConfiguration' - Describes a notification topic and its status. Notification topics are
-- used for publishing ElastiCache events to subscribers using Amazon
-- Simple Notification Service (SNS).
--
-- 'numCacheNodes', 'cacheCluster_numCacheNodes' - The number of cache nodes in the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
--
-- 'pendingModifiedValues', 'cacheCluster_pendingModifiedValues' - Undocumented member.
--
-- 'preferredAvailabilityZone', 'cacheCluster_preferredAvailabilityZone' - The name of the Availability Zone in which the cluster is located or
-- \"Multiple\" if the cache nodes are located in different Availability
-- Zones.
--
-- 'preferredMaintenanceWindow', 'cacheCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
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
-- 'preferredOutpostArn', 'cacheCluster_preferredOutpostArn' - The outpost ARN in which the cache cluster is created.
--
-- 'replicationGroupId', 'cacheCluster_replicationGroupId' - The replication group to which this cluster belongs. If this field is
-- empty, the cluster is not associated with any replication group.
--
-- 'replicationGroupLogDeliveryEnabled', 'cacheCluster_replicationGroupLogDeliveryEnabled' - A boolean value indicating whether log delivery is enabled for the
-- replication group.
--
-- 'securityGroups', 'cacheCluster_securityGroups' - A list of VPC Security Groups associated with the cluster.
--
-- 'snapshotRetentionLimit', 'cacheCluster_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
--
-- 'snapshotWindow', 'cacheCluster_snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your cluster.
--
-- Example: @05:00-09:00@
--
-- 'transitEncryptionEnabled', 'cacheCluster_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- 'transitEncryptionMode', 'cacheCluster_transitEncryptionMode' - A setting that allows you to migrate your clients to use in-transit
-- encryption, with no downtime.
newCacheCluster ::
  CacheCluster
newCacheCluster =
  CacheCluster'
    { arn = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      authTokenEnabled = Prelude.Nothing,
      authTokenLastModifiedDate = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      cacheClusterCreateTime = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      cacheClusterStatus = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      cacheNodes = Prelude.Nothing,
      cacheParameterGroup = Prelude.Nothing,
      cacheSecurityGroups = Prelude.Nothing,
      cacheSubnetGroupName = Prelude.Nothing,
      clientDownloadLandingPage = Prelude.Nothing,
      configurationEndpoint = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      ipDiscovery = Prelude.Nothing,
      logDeliveryConfigurations = Prelude.Nothing,
      networkType = Prelude.Nothing,
      notificationConfiguration = Prelude.Nothing,
      numCacheNodes = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      preferredAvailabilityZone = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      preferredOutpostArn = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      replicationGroupLogDeliveryEnabled = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing,
      transitEncryptionMode = Prelude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the cache cluster.
cacheCluster_arn :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_arn = Lens.lens (\CacheCluster' {arn} -> arn) (\s@CacheCluster' {} a -> s {arn = a} :: CacheCluster)

-- | A flag that enables encryption at-rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- cluster is created. To enable at-rest encryption on a cluster you must
-- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
cacheCluster_atRestEncryptionEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_atRestEncryptionEnabled = Lens.lens (\CacheCluster' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@CacheCluster' {} a -> s {atRestEncryptionEnabled = a} :: CacheCluster)

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
cacheCluster_authTokenEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_authTokenEnabled = Lens.lens (\CacheCluster' {authTokenEnabled} -> authTokenEnabled) (\s@CacheCluster' {} a -> s {authTokenEnabled = a} :: CacheCluster)

-- | The date the auth token was last modified
cacheCluster_authTokenLastModifiedDate :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.UTCTime)
cacheCluster_authTokenLastModifiedDate = Lens.lens (\CacheCluster' {authTokenLastModifiedDate} -> authTokenLastModifiedDate) (\s@CacheCluster' {} a -> s {authTokenLastModifiedDate = a} :: CacheCluster) Prelude.. Lens.mapping Data._Time

-- | If you are running Redis engine version 6.0 or later, set this
-- parameter to yes if you want to opt-in to the next auto minor version
-- upgrade campaign. This parameter is disabled for previous versions.
cacheCluster_autoMinorVersionUpgrade :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_autoMinorVersionUpgrade = Lens.lens (\CacheCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CacheCluster' {} a -> s {autoMinorVersionUpgrade = a} :: CacheCluster)

-- | The date and time when the cluster was created.
cacheCluster_cacheClusterCreateTime :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.UTCTime)
cacheCluster_cacheClusterCreateTime = Lens.lens (\CacheCluster' {cacheClusterCreateTime} -> cacheClusterCreateTime) (\s@CacheCluster' {} a -> s {cacheClusterCreateTime = a} :: CacheCluster) Prelude.. Lens.mapping Data._Time

-- | The user-supplied identifier of the cluster. This identifier is a unique
-- key that identifies a cluster.
cacheCluster_cacheClusterId :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheClusterId = Lens.lens (\CacheCluster' {cacheClusterId} -> cacheClusterId) (\s@CacheCluster' {} a -> s {cacheClusterId = a} :: CacheCluster)

-- | The current state of this cluster, one of the following values:
-- @available@, @creating@, @deleted@, @deleting@, @incompatible-network@,
-- @modifying@, @rebooting cluster nodes@, @restore-failed@, or
-- @snapshotting@.
cacheCluster_cacheClusterStatus :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheClusterStatus = Lens.lens (\CacheCluster' {cacheClusterStatus} -> cacheClusterStatus) (\s@CacheCluster' {} a -> s {cacheClusterStatus = a} :: CacheCluster)

-- | The name of the compute and memory capacity node type for the cluster.
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
--         5.0.6 onward and for Memcached engine version 1.5.16 onward):
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
--         __T4g node types__ (available only for Redis engine version
--         5.0.6 onward and Memcached engine version 1.5.16 onward):
--         @cache.t4g.micro@, @cache.t4g.small@, @cache.t4g.medium@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
--     -   Previous generation: (not recommended. Existing clusters are
--         still supported but creation of new clusters is not supported
--         for these types.)
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
cacheCluster_cacheNodeType :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheNodeType = Lens.lens (\CacheCluster' {cacheNodeType} -> cacheNodeType) (\s@CacheCluster' {} a -> s {cacheNodeType = a} :: CacheCluster)

-- | A list of cache nodes that are members of the cluster.
cacheCluster_cacheNodes :: Lens.Lens' CacheCluster (Prelude.Maybe [CacheNode])
cacheCluster_cacheNodes = Lens.lens (\CacheCluster' {cacheNodes} -> cacheNodes) (\s@CacheCluster' {} a -> s {cacheNodes = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | Status of the cache parameter group.
cacheCluster_cacheParameterGroup :: Lens.Lens' CacheCluster (Prelude.Maybe CacheParameterGroupStatus)
cacheCluster_cacheParameterGroup = Lens.lens (\CacheCluster' {cacheParameterGroup} -> cacheParameterGroup) (\s@CacheCluster' {} a -> s {cacheParameterGroup = a} :: CacheCluster)

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
cacheCluster_cacheSecurityGroups :: Lens.Lens' CacheCluster (Prelude.Maybe [CacheSecurityGroupMembership])
cacheCluster_cacheSecurityGroups = Lens.lens (\CacheCluster' {cacheSecurityGroups} -> cacheSecurityGroups) (\s@CacheCluster' {} a -> s {cacheSecurityGroups = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | The name of the cache subnet group associated with the cluster.
cacheCluster_cacheSubnetGroupName :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheSubnetGroupName = Lens.lens (\CacheCluster' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@CacheCluster' {} a -> s {cacheSubnetGroupName = a} :: CacheCluster)

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
cacheCluster_clientDownloadLandingPage :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_clientDownloadLandingPage = Lens.lens (\CacheCluster' {clientDownloadLandingPage} -> clientDownloadLandingPage) (\s@CacheCluster' {} a -> s {clientDownloadLandingPage = a} :: CacheCluster)

-- | Represents a Memcached cluster endpoint which can be used by an
-- application to connect to any node in the cluster. The configuration
-- endpoint will always have @.cfg@ in it.
--
-- Example: @mem-3.9dvc4r@/@.cfg@/@.usw2.cache.amazonaws.com:11211@
cacheCluster_configurationEndpoint :: Lens.Lens' CacheCluster (Prelude.Maybe Endpoint)
cacheCluster_configurationEndpoint = Lens.lens (\CacheCluster' {configurationEndpoint} -> configurationEndpoint) (\s@CacheCluster' {} a -> s {configurationEndpoint = a} :: CacheCluster)

-- | The name of the cache engine (@memcached@ or @redis@) to be used for
-- this cluster.
cacheCluster_engine :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_engine = Lens.lens (\CacheCluster' {engine} -> engine) (\s@CacheCluster' {} a -> s {engine = a} :: CacheCluster)

-- | The version of the cache engine that is used in this cluster.
cacheCluster_engineVersion :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_engineVersion = Lens.lens (\CacheCluster' {engineVersion} -> engineVersion) (\s@CacheCluster' {} a -> s {engineVersion = a} :: CacheCluster)

-- | The network type associated with the cluster, either @ipv4@ | @ipv6@.
-- IPv6 is supported for workloads using Redis engine version 6.2 onward or
-- Memcached engine version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
cacheCluster_ipDiscovery :: Lens.Lens' CacheCluster (Prelude.Maybe IpDiscovery)
cacheCluster_ipDiscovery = Lens.lens (\CacheCluster' {ipDiscovery} -> ipDiscovery) (\s@CacheCluster' {} a -> s {ipDiscovery = a} :: CacheCluster)

-- | Returns the destination, format and type of the logs.
cacheCluster_logDeliveryConfigurations :: Lens.Lens' CacheCluster (Prelude.Maybe [LogDeliveryConfiguration])
cacheCluster_logDeliveryConfigurations = Lens.lens (\CacheCluster' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@CacheCluster' {} a -> s {logDeliveryConfigurations = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | Must be either @ipv4@ | @ipv6@ | @dual_stack@. IPv6 is supported for
-- workloads using Redis engine version 6.2 onward or Memcached engine
-- version 1.6.6 on all instances built on the
-- <https://aws.amazon.com/ec2/nitro/ Nitro system>.
cacheCluster_networkType :: Lens.Lens' CacheCluster (Prelude.Maybe NetworkType)
cacheCluster_networkType = Lens.lens (\CacheCluster' {networkType} -> networkType) (\s@CacheCluster' {} a -> s {networkType = a} :: CacheCluster)

-- | Describes a notification topic and its status. Notification topics are
-- used for publishing ElastiCache events to subscribers using Amazon
-- Simple Notification Service (SNS).
cacheCluster_notificationConfiguration :: Lens.Lens' CacheCluster (Prelude.Maybe NotificationConfiguration)
cacheCluster_notificationConfiguration = Lens.lens (\CacheCluster' {notificationConfiguration} -> notificationConfiguration) (\s@CacheCluster' {} a -> s {notificationConfiguration = a} :: CacheCluster)

-- | The number of cache nodes in the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
cacheCluster_numCacheNodes :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Int)
cacheCluster_numCacheNodes = Lens.lens (\CacheCluster' {numCacheNodes} -> numCacheNodes) (\s@CacheCluster' {} a -> s {numCacheNodes = a} :: CacheCluster)

-- | Undocumented member.
cacheCluster_pendingModifiedValues :: Lens.Lens' CacheCluster (Prelude.Maybe PendingModifiedValues)
cacheCluster_pendingModifiedValues = Lens.lens (\CacheCluster' {pendingModifiedValues} -> pendingModifiedValues) (\s@CacheCluster' {} a -> s {pendingModifiedValues = a} :: CacheCluster)

-- | The name of the Availability Zone in which the cluster is located or
-- \"Multiple\" if the cache nodes are located in different Availability
-- Zones.
cacheCluster_preferredAvailabilityZone :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_preferredAvailabilityZone = Lens.lens (\CacheCluster' {preferredAvailabilityZone} -> preferredAvailabilityZone) (\s@CacheCluster' {} a -> s {preferredAvailabilityZone = a} :: CacheCluster)

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
cacheCluster_preferredMaintenanceWindow :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_preferredMaintenanceWindow = Lens.lens (\CacheCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CacheCluster' {} a -> s {preferredMaintenanceWindow = a} :: CacheCluster)

-- | The outpost ARN in which the cache cluster is created.
cacheCluster_preferredOutpostArn :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_preferredOutpostArn = Lens.lens (\CacheCluster' {preferredOutpostArn} -> preferredOutpostArn) (\s@CacheCluster' {} a -> s {preferredOutpostArn = a} :: CacheCluster)

-- | The replication group to which this cluster belongs. If this field is
-- empty, the cluster is not associated with any replication group.
cacheCluster_replicationGroupId :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_replicationGroupId = Lens.lens (\CacheCluster' {replicationGroupId} -> replicationGroupId) (\s@CacheCluster' {} a -> s {replicationGroupId = a} :: CacheCluster)

-- | A boolean value indicating whether log delivery is enabled for the
-- replication group.
cacheCluster_replicationGroupLogDeliveryEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_replicationGroupLogDeliveryEnabled = Lens.lens (\CacheCluster' {replicationGroupLogDeliveryEnabled} -> replicationGroupLogDeliveryEnabled) (\s@CacheCluster' {} a -> s {replicationGroupLogDeliveryEnabled = a} :: CacheCluster)

-- | A list of VPC Security Groups associated with the cluster.
cacheCluster_securityGroups :: Lens.Lens' CacheCluster (Prelude.Maybe [SecurityGroupMembership])
cacheCluster_securityGroups = Lens.lens (\CacheCluster' {securityGroups} -> securityGroups) (\s@CacheCluster' {} a -> s {securityGroups = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
cacheCluster_snapshotRetentionLimit :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Int)
cacheCluster_snapshotRetentionLimit = Lens.lens (\CacheCluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@CacheCluster' {} a -> s {snapshotRetentionLimit = a} :: CacheCluster)

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your cluster.
--
-- Example: @05:00-09:00@
cacheCluster_snapshotWindow :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_snapshotWindow = Lens.lens (\CacheCluster' {snapshotWindow} -> snapshotWindow) (\s@CacheCluster' {} a -> s {snapshotWindow = a} :: CacheCluster)

-- | A flag that enables in-transit encryption when set to @true@.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
cacheCluster_transitEncryptionEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_transitEncryptionEnabled = Lens.lens (\CacheCluster' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@CacheCluster' {} a -> s {transitEncryptionEnabled = a} :: CacheCluster)

-- | A setting that allows you to migrate your clients to use in-transit
-- encryption, with no downtime.
cacheCluster_transitEncryptionMode :: Lens.Lens' CacheCluster (Prelude.Maybe TransitEncryptionMode)
cacheCluster_transitEncryptionMode = Lens.lens (\CacheCluster' {transitEncryptionMode} -> transitEncryptionMode) (\s@CacheCluster' {} a -> s {transitEncryptionMode = a} :: CacheCluster)

instance Data.FromXML CacheCluster where
  parseXML x =
    CacheCluster'
      Prelude.<$> (x Data..@? "ARN")
      Prelude.<*> (x Data..@? "AtRestEncryptionEnabled")
      Prelude.<*> (x Data..@? "AuthTokenEnabled")
      Prelude.<*> (x Data..@? "AuthTokenLastModifiedDate")
      Prelude.<*> (x Data..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Data..@? "CacheClusterCreateTime")
      Prelude.<*> (x Data..@? "CacheClusterId")
      Prelude.<*> (x Data..@? "CacheClusterStatus")
      Prelude.<*> (x Data..@? "CacheNodeType")
      Prelude.<*> ( x Data..@? "CacheNodes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CacheNode")
                  )
      Prelude.<*> (x Data..@? "CacheParameterGroup")
      Prelude.<*> ( x
                      Data..@? "CacheSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "CacheSecurityGroup")
                  )
      Prelude.<*> (x Data..@? "CacheSubnetGroupName")
      Prelude.<*> (x Data..@? "ClientDownloadLandingPage")
      Prelude.<*> (x Data..@? "ConfigurationEndpoint")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "IpDiscovery")
      Prelude.<*> ( x
                      Data..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "LogDeliveryConfiguration")
                  )
      Prelude.<*> (x Data..@? "NetworkType")
      Prelude.<*> (x Data..@? "NotificationConfiguration")
      Prelude.<*> (x Data..@? "NumCacheNodes")
      Prelude.<*> (x Data..@? "PendingModifiedValues")
      Prelude.<*> (x Data..@? "PreferredAvailabilityZone")
      Prelude.<*> (x Data..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Data..@? "PreferredOutpostArn")
      Prelude.<*> (x Data..@? "ReplicationGroupId")
      Prelude.<*> (x Data..@? "ReplicationGroupLogDeliveryEnabled")
      Prelude.<*> ( x Data..@? "SecurityGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "SnapshotRetentionLimit")
      Prelude.<*> (x Data..@? "SnapshotWindow")
      Prelude.<*> (x Data..@? "TransitEncryptionEnabled")
      Prelude.<*> (x Data..@? "TransitEncryptionMode")

instance Prelude.Hashable CacheCluster where
  hashWithSalt _salt CacheCluster' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` authTokenEnabled
      `Prelude.hashWithSalt` authTokenLastModifiedDate
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` cacheClusterCreateTime
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` cacheClusterStatus
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` cacheNodes
      `Prelude.hashWithSalt` cacheParameterGroup
      `Prelude.hashWithSalt` cacheSecurityGroups
      `Prelude.hashWithSalt` cacheSubnetGroupName
      `Prelude.hashWithSalt` clientDownloadLandingPage
      `Prelude.hashWithSalt` configurationEndpoint
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` ipDiscovery
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` notificationConfiguration
      `Prelude.hashWithSalt` numCacheNodes
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` preferredAvailabilityZone
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` preferredOutpostArn
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` replicationGroupLogDeliveryEnabled
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` transitEncryptionMode

instance Prelude.NFData CacheCluster where
  rnf CacheCluster' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf authTokenEnabled
      `Prelude.seq` Prelude.rnf authTokenLastModifiedDate
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf cacheClusterCreateTime
      `Prelude.seq` Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf cacheClusterStatus
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf cacheNodes
      `Prelude.seq` Prelude.rnf cacheParameterGroup
      `Prelude.seq` Prelude.rnf cacheSecurityGroups
      `Prelude.seq` Prelude.rnf cacheSubnetGroupName
      `Prelude.seq` Prelude.rnf clientDownloadLandingPage
      `Prelude.seq` Prelude.rnf configurationEndpoint
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf ipDiscovery
      `Prelude.seq` Prelude.rnf
        logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf networkType
      `Prelude.seq` Prelude.rnf
        notificationConfiguration
      `Prelude.seq` Prelude.rnf numCacheNodes
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        preferredAvailabilityZone
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        preferredOutpostArn
      `Prelude.seq` Prelude.rnf
        replicationGroupId
      `Prelude.seq` Prelude.rnf
        replicationGroupLogDeliveryEnabled
      `Prelude.seq` Prelude.rnf
        securityGroups
      `Prelude.seq` Prelude.rnf
        snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf
        snapshotWindow
      `Prelude.seq` Prelude.rnf
        transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf
        transitEncryptionMode
