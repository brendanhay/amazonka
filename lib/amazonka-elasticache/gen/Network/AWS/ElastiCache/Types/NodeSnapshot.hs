{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.NodeSnapshot
  ( NodeSnapshot (..)
  -- * Smart constructor
  , mkNodeSnapshot
  -- * Lenses
  , nsCacheClusterId
  , nsCacheNodeCreateTime
  , nsCacheNodeId
  , nsCacheSize
  , nsNodeGroupConfiguration
  , nsNodeGroupId
  , nsSnapshotCreateTime
  ) where

import qualified Network.AWS.ElastiCache.Types.NodeGroupConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an individual cache node in a snapshot of a cluster.
--
-- /See:/ 'mkNodeSnapshot' smart constructor.
data NodeSnapshot = NodeSnapshot'
  { cacheClusterId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the source cluster.
  , cacheNodeCreateTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the cache node was created in the source cluster.
  , cacheNodeId :: Core.Maybe Core.Text
    -- ^ The cache node identifier for the node in the source cluster.
  , cacheSize :: Core.Maybe Core.Text
    -- ^ The size of the cache on the source cache node.
  , nodeGroupConfiguration :: Core.Maybe Types.NodeGroupConfiguration
    -- ^ The configuration for the source node group (shard).
  , nodeGroupId :: Core.Maybe Core.Text
    -- ^ A unique identifier for the source node group (shard).
  , snapshotCreateTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time when the source node's metadata and cache data set was obtained for the snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'NodeSnapshot' value with any optional fields omitted.
mkNodeSnapshot
    :: NodeSnapshot
mkNodeSnapshot
  = NodeSnapshot'{cacheClusterId = Core.Nothing,
                  cacheNodeCreateTime = Core.Nothing, cacheNodeId = Core.Nothing,
                  cacheSize = Core.Nothing, nodeGroupConfiguration = Core.Nothing,
                  nodeGroupId = Core.Nothing, snapshotCreateTime = Core.Nothing}

-- | A unique identifier for the source cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheClusterId :: Lens.Lens' NodeSnapshot (Core.Maybe Core.Text)
nsCacheClusterId = Lens.field @"cacheClusterId"
{-# INLINEABLE nsCacheClusterId #-}
{-# DEPRECATED cacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead"  #-}

-- | The date and time when the cache node was created in the source cluster.
--
-- /Note:/ Consider using 'cacheNodeCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheNodeCreateTime :: Lens.Lens' NodeSnapshot (Core.Maybe Core.UTCTime)
nsCacheNodeCreateTime = Lens.field @"cacheNodeCreateTime"
{-# INLINEABLE nsCacheNodeCreateTime #-}
{-# DEPRECATED cacheNodeCreateTime "Use generic-lens or generic-optics with 'cacheNodeCreateTime' instead"  #-}

-- | The cache node identifier for the node in the source cluster.
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheNodeId :: Lens.Lens' NodeSnapshot (Core.Maybe Core.Text)
nsCacheNodeId = Lens.field @"cacheNodeId"
{-# INLINEABLE nsCacheNodeId #-}
{-# DEPRECATED cacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead"  #-}

-- | The size of the cache on the source cache node.
--
-- /Note:/ Consider using 'cacheSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheSize :: Lens.Lens' NodeSnapshot (Core.Maybe Core.Text)
nsCacheSize = Lens.field @"cacheSize"
{-# INLINEABLE nsCacheSize #-}
{-# DEPRECATED cacheSize "Use generic-lens or generic-optics with 'cacheSize' instead"  #-}

-- | The configuration for the source node group (shard).
--
-- /Note:/ Consider using 'nodeGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsNodeGroupConfiguration :: Lens.Lens' NodeSnapshot (Core.Maybe Types.NodeGroupConfiguration)
nsNodeGroupConfiguration = Lens.field @"nodeGroupConfiguration"
{-# INLINEABLE nsNodeGroupConfiguration #-}
{-# DEPRECATED nodeGroupConfiguration "Use generic-lens or generic-optics with 'nodeGroupConfiguration' instead"  #-}

-- | A unique identifier for the source node group (shard).
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsNodeGroupId :: Lens.Lens' NodeSnapshot (Core.Maybe Core.Text)
nsNodeGroupId = Lens.field @"nodeGroupId"
{-# INLINEABLE nsNodeGroupId #-}
{-# DEPRECATED nodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead"  #-}

-- | The date and time when the source node's metadata and cache data set was obtained for the snapshot.
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsSnapshotCreateTime :: Lens.Lens' NodeSnapshot (Core.Maybe Core.UTCTime)
nsSnapshotCreateTime = Lens.field @"snapshotCreateTime"
{-# INLINEABLE nsSnapshotCreateTime #-}
{-# DEPRECATED snapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead"  #-}

instance Core.FromXML NodeSnapshot where
        parseXML x
          = NodeSnapshot' Core.<$>
              (x Core..@? "CacheClusterId") Core.<*>
                x Core..@? "CacheNodeCreateTime"
                Core.<*> x Core..@? "CacheNodeId"
                Core.<*> x Core..@? "CacheSize"
                Core.<*> x Core..@? "NodeGroupConfiguration"
                Core.<*> x Core..@? "NodeGroupId"
                Core.<*> x Core..@? "SnapshotCreateTime"
