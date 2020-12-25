{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeSnapshot
  ( NodeSnapshot (..),

    -- * Smart constructor
    mkNodeSnapshot,

    -- * Lenses
    nsCacheClusterId,
    nsCacheNodeCreateTime,
    nsCacheNodeId,
    nsCacheSize,
    nsNodeGroupConfiguration,
    nsNodeGroupId,
    nsSnapshotCreateTime,
  )
where

import qualified Network.AWS.ElastiCache.Types.CacheClusterId as Types
import qualified Network.AWS.ElastiCache.Types.CacheNodeId as Types
import qualified Network.AWS.ElastiCache.Types.CacheSize as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroupConfiguration as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an individual cache node in a snapshot of a cluster.
--
-- /See:/ 'mkNodeSnapshot' smart constructor.
data NodeSnapshot = NodeSnapshot'
  { -- | A unique identifier for the source cluster.
    cacheClusterId :: Core.Maybe Types.CacheClusterId,
    -- | The date and time when the cache node was created in the source cluster.
    cacheNodeCreateTime :: Core.Maybe Core.UTCTime,
    -- | The cache node identifier for the node in the source cluster.
    cacheNodeId :: Core.Maybe Types.CacheNodeId,
    -- | The size of the cache on the source cache node.
    cacheSize :: Core.Maybe Types.CacheSize,
    -- | The configuration for the source node group (shard).
    nodeGroupConfiguration :: Core.Maybe Types.NodeGroupConfiguration,
    -- | A unique identifier for the source node group (shard).
    nodeGroupId :: Core.Maybe Types.NodeGroupId,
    -- | The date and time when the source node's metadata and cache data set was obtained for the snapshot.
    snapshotCreateTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NodeSnapshot' value with any optional fields omitted.
mkNodeSnapshot ::
  NodeSnapshot
mkNodeSnapshot =
  NodeSnapshot'
    { cacheClusterId = Core.Nothing,
      cacheNodeCreateTime = Core.Nothing,
      cacheNodeId = Core.Nothing,
      cacheSize = Core.Nothing,
      nodeGroupConfiguration = Core.Nothing,
      nodeGroupId = Core.Nothing,
      snapshotCreateTime = Core.Nothing
    }

-- | A unique identifier for the source cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheClusterId :: Lens.Lens' NodeSnapshot (Core.Maybe Types.CacheClusterId)
nsCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED nsCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The date and time when the cache node was created in the source cluster.
--
-- /Note:/ Consider using 'cacheNodeCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheNodeCreateTime :: Lens.Lens' NodeSnapshot (Core.Maybe Core.UTCTime)
nsCacheNodeCreateTime = Lens.field @"cacheNodeCreateTime"
{-# DEPRECATED nsCacheNodeCreateTime "Use generic-lens or generic-optics with 'cacheNodeCreateTime' instead." #-}

-- | The cache node identifier for the node in the source cluster.
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheNodeId :: Lens.Lens' NodeSnapshot (Core.Maybe Types.CacheNodeId)
nsCacheNodeId = Lens.field @"cacheNodeId"
{-# DEPRECATED nsCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | The size of the cache on the source cache node.
--
-- /Note:/ Consider using 'cacheSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheSize :: Lens.Lens' NodeSnapshot (Core.Maybe Types.CacheSize)
nsCacheSize = Lens.field @"cacheSize"
{-# DEPRECATED nsCacheSize "Use generic-lens or generic-optics with 'cacheSize' instead." #-}

-- | The configuration for the source node group (shard).
--
-- /Note:/ Consider using 'nodeGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsNodeGroupConfiguration :: Lens.Lens' NodeSnapshot (Core.Maybe Types.NodeGroupConfiguration)
nsNodeGroupConfiguration = Lens.field @"nodeGroupConfiguration"
{-# DEPRECATED nsNodeGroupConfiguration "Use generic-lens or generic-optics with 'nodeGroupConfiguration' instead." #-}

-- | A unique identifier for the source node group (shard).
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsNodeGroupId :: Lens.Lens' NodeSnapshot (Core.Maybe Types.NodeGroupId)
nsNodeGroupId = Lens.field @"nodeGroupId"
{-# DEPRECATED nsNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The date and time when the source node's metadata and cache data set was obtained for the snapshot.
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsSnapshotCreateTime :: Lens.Lens' NodeSnapshot (Core.Maybe Core.UTCTime)
nsSnapshotCreateTime = Lens.field @"snapshotCreateTime"
{-# DEPRECATED nsSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

instance Core.FromXML NodeSnapshot where
  parseXML x =
    NodeSnapshot'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "CacheNodeCreateTime")
      Core.<*> (x Core..@? "CacheNodeId")
      Core.<*> (x Core..@? "CacheSize")
      Core.<*> (x Core..@? "NodeGroupConfiguration")
      Core.<*> (x Core..@? "NodeGroupId")
      Core.<*> (x Core..@? "SnapshotCreateTime")
