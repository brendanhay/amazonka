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
-- Module      : Amazonka.ElastiCache.Types.CacheNode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types.Endpoint
import qualified Amazonka.Prelude as Prelude

-- | Represents an individual cache node within a cluster. Each cache node
-- runs its own instance of the cluster\'s protocol-compliant caching
-- software - either Memcached or Redis.
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
-- /See:/ 'newCacheNode' smart constructor.
data CacheNode = CacheNode'
  { -- | The date and time when the cache node was created.
    cacheNodeCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | The cache node identifier. A node ID is a numeric identifier (0001,
    -- 0002, etc.). The combination of cluster ID and node ID uniquely
    -- identifies every cache node used in a customer\'s Amazon account.
    cacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The current state of this cache node, one of the following values:
    -- @available@, @creating@, @rebooting@, or @deleting@.
    cacheNodeStatus :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone where this node was created and now resides.
    customerAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The customer outpost ARN of the cache node.
    customerOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | The hostname for connecting to this cache node.
    endpoint :: Prelude.Maybe Endpoint,
    -- | The status of the parameter group applied to this cache node.
    parameterGroupStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of the primary node to which this read replica node is
    -- synchronized. If this field is empty, this node is not associated with a
    -- primary cluster.
    sourceCacheNodeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cacheNodeCreateTime', 'cacheNode_cacheNodeCreateTime' - The date and time when the cache node was created.
--
-- 'cacheNodeId', 'cacheNode_cacheNodeId' - The cache node identifier. A node ID is a numeric identifier (0001,
-- 0002, etc.). The combination of cluster ID and node ID uniquely
-- identifies every cache node used in a customer\'s Amazon account.
--
-- 'cacheNodeStatus', 'cacheNode_cacheNodeStatus' - The current state of this cache node, one of the following values:
-- @available@, @creating@, @rebooting@, or @deleting@.
--
-- 'customerAvailabilityZone', 'cacheNode_customerAvailabilityZone' - The Availability Zone where this node was created and now resides.
--
-- 'customerOutpostArn', 'cacheNode_customerOutpostArn' - The customer outpost ARN of the cache node.
--
-- 'endpoint', 'cacheNode_endpoint' - The hostname for connecting to this cache node.
--
-- 'parameterGroupStatus', 'cacheNode_parameterGroupStatus' - The status of the parameter group applied to this cache node.
--
-- 'sourceCacheNodeId', 'cacheNode_sourceCacheNodeId' - The ID of the primary node to which this read replica node is
-- synchronized. If this field is empty, this node is not associated with a
-- primary cluster.
newCacheNode ::
  CacheNode
newCacheNode =
  CacheNode'
    { cacheNodeCreateTime = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing,
      cacheNodeStatus = Prelude.Nothing,
      customerAvailabilityZone = Prelude.Nothing,
      customerOutpostArn = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      parameterGroupStatus = Prelude.Nothing,
      sourceCacheNodeId = Prelude.Nothing
    }

-- | The date and time when the cache node was created.
cacheNode_cacheNodeCreateTime :: Lens.Lens' CacheNode (Prelude.Maybe Prelude.UTCTime)
cacheNode_cacheNodeCreateTime = Lens.lens (\CacheNode' {cacheNodeCreateTime} -> cacheNodeCreateTime) (\s@CacheNode' {} a -> s {cacheNodeCreateTime = a} :: CacheNode) Prelude.. Lens.mapping Data._Time

-- | The cache node identifier. A node ID is a numeric identifier (0001,
-- 0002, etc.). The combination of cluster ID and node ID uniquely
-- identifies every cache node used in a customer\'s Amazon account.
cacheNode_cacheNodeId :: Lens.Lens' CacheNode (Prelude.Maybe Prelude.Text)
cacheNode_cacheNodeId = Lens.lens (\CacheNode' {cacheNodeId} -> cacheNodeId) (\s@CacheNode' {} a -> s {cacheNodeId = a} :: CacheNode)

-- | The current state of this cache node, one of the following values:
-- @available@, @creating@, @rebooting@, or @deleting@.
cacheNode_cacheNodeStatus :: Lens.Lens' CacheNode (Prelude.Maybe Prelude.Text)
cacheNode_cacheNodeStatus = Lens.lens (\CacheNode' {cacheNodeStatus} -> cacheNodeStatus) (\s@CacheNode' {} a -> s {cacheNodeStatus = a} :: CacheNode)

-- | The Availability Zone where this node was created and now resides.
cacheNode_customerAvailabilityZone :: Lens.Lens' CacheNode (Prelude.Maybe Prelude.Text)
cacheNode_customerAvailabilityZone = Lens.lens (\CacheNode' {customerAvailabilityZone} -> customerAvailabilityZone) (\s@CacheNode' {} a -> s {customerAvailabilityZone = a} :: CacheNode)

-- | The customer outpost ARN of the cache node.
cacheNode_customerOutpostArn :: Lens.Lens' CacheNode (Prelude.Maybe Prelude.Text)
cacheNode_customerOutpostArn = Lens.lens (\CacheNode' {customerOutpostArn} -> customerOutpostArn) (\s@CacheNode' {} a -> s {customerOutpostArn = a} :: CacheNode)

-- | The hostname for connecting to this cache node.
cacheNode_endpoint :: Lens.Lens' CacheNode (Prelude.Maybe Endpoint)
cacheNode_endpoint = Lens.lens (\CacheNode' {endpoint} -> endpoint) (\s@CacheNode' {} a -> s {endpoint = a} :: CacheNode)

-- | The status of the parameter group applied to this cache node.
cacheNode_parameterGroupStatus :: Lens.Lens' CacheNode (Prelude.Maybe Prelude.Text)
cacheNode_parameterGroupStatus = Lens.lens (\CacheNode' {parameterGroupStatus} -> parameterGroupStatus) (\s@CacheNode' {} a -> s {parameterGroupStatus = a} :: CacheNode)

-- | The ID of the primary node to which this read replica node is
-- synchronized. If this field is empty, this node is not associated with a
-- primary cluster.
cacheNode_sourceCacheNodeId :: Lens.Lens' CacheNode (Prelude.Maybe Prelude.Text)
cacheNode_sourceCacheNodeId = Lens.lens (\CacheNode' {sourceCacheNodeId} -> sourceCacheNodeId) (\s@CacheNode' {} a -> s {sourceCacheNodeId = a} :: CacheNode)

instance Data.FromXML CacheNode where
  parseXML x =
    CacheNode'
      Prelude.<$> (x Data..@? "CacheNodeCreateTime")
      Prelude.<*> (x Data..@? "CacheNodeId")
      Prelude.<*> (x Data..@? "CacheNodeStatus")
      Prelude.<*> (x Data..@? "CustomerAvailabilityZone")
      Prelude.<*> (x Data..@? "CustomerOutpostArn")
      Prelude.<*> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "ParameterGroupStatus")
      Prelude.<*> (x Data..@? "SourceCacheNodeId")

instance Prelude.Hashable CacheNode where
  hashWithSalt _salt CacheNode' {..} =
    _salt
      `Prelude.hashWithSalt` cacheNodeCreateTime
      `Prelude.hashWithSalt` cacheNodeId
      `Prelude.hashWithSalt` cacheNodeStatus
      `Prelude.hashWithSalt` customerAvailabilityZone
      `Prelude.hashWithSalt` customerOutpostArn
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` parameterGroupStatus
      `Prelude.hashWithSalt` sourceCacheNodeId

instance Prelude.NFData CacheNode where
  rnf CacheNode' {..} =
    Prelude.rnf cacheNodeCreateTime
      `Prelude.seq` Prelude.rnf cacheNodeId
      `Prelude.seq` Prelude.rnf cacheNodeStatus
      `Prelude.seq` Prelude.rnf customerAvailabilityZone
      `Prelude.seq` Prelude.rnf customerOutpostArn
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf parameterGroupStatus
      `Prelude.seq` Prelude.rnf sourceCacheNodeId
