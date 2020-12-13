{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNode
  ( CacheNode (..),

    -- * Smart constructor
    mkCacheNode,

    -- * Lenses
    cnSourceCacheNodeId,
    cnParameterGroupStatus,
    cnCacheNodeCreateTime,
    cnCustomerAvailabilityZone,
    cnCacheNodeId,
    cnCustomerOutpostARN,
    cnCacheNodeStatus,
    cnEndpoint,
  )
where

import Network.AWS.ElastiCache.Types.Endpoint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an individual cache node within a cluster. Each cache node runs its own instance of the cluster's protocol-compliant caching software - either Memcached or Redis.
--
-- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
--
--     * General purpose:
--
--     * Current generation:
-- __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@
-- __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@
-- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
-- __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@
-- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
--
--
--     * Previous generation: (not recommended)
-- __T1 node types:__ @cache.t1.micro@
-- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
-- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
--
--
--
--
--     * Compute optimized:
--
--     * Previous generation: (not recommended)
-- __C1 node types:__ @cache.c1.xlarge@
--
--
--
--
--     * Memory optimized:
--
--     * Current generation:
-- __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward).
-- @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@
-- __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@
-- __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@
--
--
--     * Previous generation: (not recommended)
-- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
-- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
--
--
--
--
-- __Additional node type info__
--
--     * All current generation instance types are created in Amazon VPC by default.
--
--
--     * Redis append-only files (AOF) are not supported for T1 or T2 instances.
--
--
--     * Redis Multi-AZ with automatic failover is not supported on T1 instances.
--
--
--     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
--
--
-- /See:/ 'mkCacheNode' smart constructor.
data CacheNode = CacheNode'
  { -- | The ID of the primary node to which this read replica node is synchronized. If this field is empty, this node is not associated with a primary cluster.
    sourceCacheNodeId :: Lude.Maybe Lude.Text,
    -- | The status of the parameter group applied to this cache node.
    parameterGroupStatus :: Lude.Maybe Lude.Text,
    -- | The date and time when the cache node was created.
    cacheNodeCreateTime :: Lude.Maybe Lude.DateTime,
    -- | The Availability Zone where this node was created and now resides.
    customerAvailabilityZone :: Lude.Maybe Lude.Text,
    -- | The cache node identifier. A node ID is a numeric identifier (0001, 0002, etc.). The combination of cluster ID and node ID uniquely identifies every cache node used in a customer's AWS account.
    cacheNodeId :: Lude.Maybe Lude.Text,
    -- | The customer outpost ARN of the cache node.
    customerOutpostARN :: Lude.Maybe Lude.Text,
    -- | The current state of this cache node, one of the following values: @available@ , @creating@ , @rebooting@ , or @deleting@ .
    cacheNodeStatus :: Lude.Maybe Lude.Text,
    -- | The hostname for connecting to this cache node.
    endpoint :: Lude.Maybe Endpoint
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheNode' with the minimum fields required to make a request.
--
-- * 'sourceCacheNodeId' - The ID of the primary node to which this read replica node is synchronized. If this field is empty, this node is not associated with a primary cluster.
-- * 'parameterGroupStatus' - The status of the parameter group applied to this cache node.
-- * 'cacheNodeCreateTime' - The date and time when the cache node was created.
-- * 'customerAvailabilityZone' - The Availability Zone where this node was created and now resides.
-- * 'cacheNodeId' - The cache node identifier. A node ID is a numeric identifier (0001, 0002, etc.). The combination of cluster ID and node ID uniquely identifies every cache node used in a customer's AWS account.
-- * 'customerOutpostARN' - The customer outpost ARN of the cache node.
-- * 'cacheNodeStatus' - The current state of this cache node, one of the following values: @available@ , @creating@ , @rebooting@ , or @deleting@ .
-- * 'endpoint' - The hostname for connecting to this cache node.
mkCacheNode ::
  CacheNode
mkCacheNode =
  CacheNode'
    { sourceCacheNodeId = Lude.Nothing,
      parameterGroupStatus = Lude.Nothing,
      cacheNodeCreateTime = Lude.Nothing,
      customerAvailabilityZone = Lude.Nothing,
      cacheNodeId = Lude.Nothing,
      customerOutpostARN = Lude.Nothing,
      cacheNodeStatus = Lude.Nothing,
      endpoint = Lude.Nothing
    }

-- | The ID of the primary node to which this read replica node is synchronized. If this field is empty, this node is not associated with a primary cluster.
--
-- /Note:/ Consider using 'sourceCacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnSourceCacheNodeId :: Lens.Lens' CacheNode (Lude.Maybe Lude.Text)
cnSourceCacheNodeId = Lens.lens (sourceCacheNodeId :: CacheNode -> Lude.Maybe Lude.Text) (\s a -> s {sourceCacheNodeId = a} :: CacheNode)
{-# DEPRECATED cnSourceCacheNodeId "Use generic-lens or generic-optics with 'sourceCacheNodeId' instead." #-}

-- | The status of the parameter group applied to this cache node.
--
-- /Note:/ Consider using 'parameterGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnParameterGroupStatus :: Lens.Lens' CacheNode (Lude.Maybe Lude.Text)
cnParameterGroupStatus = Lens.lens (parameterGroupStatus :: CacheNode -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupStatus = a} :: CacheNode)
{-# DEPRECATED cnParameterGroupStatus "Use generic-lens or generic-optics with 'parameterGroupStatus' instead." #-}

-- | The date and time when the cache node was created.
--
-- /Note:/ Consider using 'cacheNodeCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnCacheNodeCreateTime :: Lens.Lens' CacheNode (Lude.Maybe Lude.DateTime)
cnCacheNodeCreateTime = Lens.lens (cacheNodeCreateTime :: CacheNode -> Lude.Maybe Lude.DateTime) (\s a -> s {cacheNodeCreateTime = a} :: CacheNode)
{-# DEPRECATED cnCacheNodeCreateTime "Use generic-lens or generic-optics with 'cacheNodeCreateTime' instead." #-}

-- | The Availability Zone where this node was created and now resides.
--
-- /Note:/ Consider using 'customerAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnCustomerAvailabilityZone :: Lens.Lens' CacheNode (Lude.Maybe Lude.Text)
cnCustomerAvailabilityZone = Lens.lens (customerAvailabilityZone :: CacheNode -> Lude.Maybe Lude.Text) (\s a -> s {customerAvailabilityZone = a} :: CacheNode)
{-# DEPRECATED cnCustomerAvailabilityZone "Use generic-lens or generic-optics with 'customerAvailabilityZone' instead." #-}

-- | The cache node identifier. A node ID is a numeric identifier (0001, 0002, etc.). The combination of cluster ID and node ID uniquely identifies every cache node used in a customer's AWS account.
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnCacheNodeId :: Lens.Lens' CacheNode (Lude.Maybe Lude.Text)
cnCacheNodeId = Lens.lens (cacheNodeId :: CacheNode -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeId = a} :: CacheNode)
{-# DEPRECATED cnCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | The customer outpost ARN of the cache node.
--
-- /Note:/ Consider using 'customerOutpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnCustomerOutpostARN :: Lens.Lens' CacheNode (Lude.Maybe Lude.Text)
cnCustomerOutpostARN = Lens.lens (customerOutpostARN :: CacheNode -> Lude.Maybe Lude.Text) (\s a -> s {customerOutpostARN = a} :: CacheNode)
{-# DEPRECATED cnCustomerOutpostARN "Use generic-lens or generic-optics with 'customerOutpostARN' instead." #-}

-- | The current state of this cache node, one of the following values: @available@ , @creating@ , @rebooting@ , or @deleting@ .
--
-- /Note:/ Consider using 'cacheNodeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnCacheNodeStatus :: Lens.Lens' CacheNode (Lude.Maybe Lude.Text)
cnCacheNodeStatus = Lens.lens (cacheNodeStatus :: CacheNode -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeStatus = a} :: CacheNode)
{-# DEPRECATED cnCacheNodeStatus "Use generic-lens or generic-optics with 'cacheNodeStatus' instead." #-}

-- | The hostname for connecting to this cache node.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnEndpoint :: Lens.Lens' CacheNode (Lude.Maybe Endpoint)
cnEndpoint = Lens.lens (endpoint :: CacheNode -> Lude.Maybe Endpoint) (\s a -> s {endpoint = a} :: CacheNode)
{-# DEPRECATED cnEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

instance Lude.FromXML CacheNode where
  parseXML x =
    CacheNode'
      Lude.<$> (x Lude..@? "SourceCacheNodeId")
      Lude.<*> (x Lude..@? "ParameterGroupStatus")
      Lude.<*> (x Lude..@? "CacheNodeCreateTime")
      Lude.<*> (x Lude..@? "CustomerAvailabilityZone")
      Lude.<*> (x Lude..@? "CacheNodeId")
      Lude.<*> (x Lude..@? "CustomerOutpostArn")
      Lude.<*> (x Lude..@? "CacheNodeStatus")
      Lude.<*> (x Lude..@? "Endpoint")
