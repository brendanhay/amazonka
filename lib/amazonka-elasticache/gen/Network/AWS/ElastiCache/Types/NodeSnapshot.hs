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
    nsNodeGroupConfiguration,
    nsCacheNodeCreateTime,
    nsCacheClusterId,
    nsCacheNodeId,
    nsNodeGroupId,
    nsSnapshotCreateTime,
    nsCacheSize,
  )
where

import Network.AWS.ElastiCache.Types.NodeGroupConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an individual cache node in a snapshot of a cluster.
--
-- /See:/ 'mkNodeSnapshot' smart constructor.
data NodeSnapshot = NodeSnapshot'
  { nodeGroupConfiguration ::
      Lude.Maybe NodeGroupConfiguration,
    cacheNodeCreateTime :: Lude.Maybe Lude.DateTime,
    cacheClusterId :: Lude.Maybe Lude.Text,
    cacheNodeId :: Lude.Maybe Lude.Text,
    nodeGroupId :: Lude.Maybe Lude.Text,
    snapshotCreateTime :: Lude.Maybe Lude.DateTime,
    cacheSize :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeSnapshot' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - A unique identifier for the source cluster.
-- * 'cacheNodeCreateTime' - The date and time when the cache node was created in the source cluster.
-- * 'cacheNodeId' - The cache node identifier for the node in the source cluster.
-- * 'cacheSize' - The size of the cache on the source cache node.
-- * 'nodeGroupConfiguration' - The configuration for the source node group (shard).
-- * 'nodeGroupId' - A unique identifier for the source node group (shard).
-- * 'snapshotCreateTime' - The date and time when the source node's metadata and cache data set was obtained for the snapshot.
mkNodeSnapshot ::
  NodeSnapshot
mkNodeSnapshot =
  NodeSnapshot'
    { nodeGroupConfiguration = Lude.Nothing,
      cacheNodeCreateTime = Lude.Nothing,
      cacheClusterId = Lude.Nothing,
      cacheNodeId = Lude.Nothing,
      nodeGroupId = Lude.Nothing,
      snapshotCreateTime = Lude.Nothing,
      cacheSize = Lude.Nothing
    }

-- | The configuration for the source node group (shard).
--
-- /Note:/ Consider using 'nodeGroupConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsNodeGroupConfiguration :: Lens.Lens' NodeSnapshot (Lude.Maybe NodeGroupConfiguration)
nsNodeGroupConfiguration = Lens.lens (nodeGroupConfiguration :: NodeSnapshot -> Lude.Maybe NodeGroupConfiguration) (\s a -> s {nodeGroupConfiguration = a} :: NodeSnapshot)
{-# DEPRECATED nsNodeGroupConfiguration "Use generic-lens or generic-optics with 'nodeGroupConfiguration' instead." #-}

-- | The date and time when the cache node was created in the source cluster.
--
-- /Note:/ Consider using 'cacheNodeCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheNodeCreateTime :: Lens.Lens' NodeSnapshot (Lude.Maybe Lude.DateTime)
nsCacheNodeCreateTime = Lens.lens (cacheNodeCreateTime :: NodeSnapshot -> Lude.Maybe Lude.DateTime) (\s a -> s {cacheNodeCreateTime = a} :: NodeSnapshot)
{-# DEPRECATED nsCacheNodeCreateTime "Use generic-lens or generic-optics with 'cacheNodeCreateTime' instead." #-}

-- | A unique identifier for the source cluster.
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheClusterId :: Lens.Lens' NodeSnapshot (Lude.Maybe Lude.Text)
nsCacheClusterId = Lens.lens (cacheClusterId :: NodeSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: NodeSnapshot)
{-# DEPRECATED nsCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The cache node identifier for the node in the source cluster.
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheNodeId :: Lens.Lens' NodeSnapshot (Lude.Maybe Lude.Text)
nsCacheNodeId = Lens.lens (cacheNodeId :: NodeSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeId = a} :: NodeSnapshot)
{-# DEPRECATED nsCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | A unique identifier for the source node group (shard).
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsNodeGroupId :: Lens.Lens' NodeSnapshot (Lude.Maybe Lude.Text)
nsNodeGroupId = Lens.lens (nodeGroupId :: NodeSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {nodeGroupId = a} :: NodeSnapshot)
{-# DEPRECATED nsNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The date and time when the source node's metadata and cache data set was obtained for the snapshot.
--
-- /Note:/ Consider using 'snapshotCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsSnapshotCreateTime :: Lens.Lens' NodeSnapshot (Lude.Maybe Lude.DateTime)
nsSnapshotCreateTime = Lens.lens (snapshotCreateTime :: NodeSnapshot -> Lude.Maybe Lude.DateTime) (\s a -> s {snapshotCreateTime = a} :: NodeSnapshot)
{-# DEPRECATED nsSnapshotCreateTime "Use generic-lens or generic-optics with 'snapshotCreateTime' instead." #-}

-- | The size of the cache on the source cache node.
--
-- /Note:/ Consider using 'cacheSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nsCacheSize :: Lens.Lens' NodeSnapshot (Lude.Maybe Lude.Text)
nsCacheSize = Lens.lens (cacheSize :: NodeSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {cacheSize = a} :: NodeSnapshot)
{-# DEPRECATED nsCacheSize "Use generic-lens or generic-optics with 'cacheSize' instead." #-}

instance Lude.FromXML NodeSnapshot where
  parseXML x =
    NodeSnapshot'
      Lude.<$> (x Lude..@? "NodeGroupConfiguration")
      Lude.<*> (x Lude..@? "CacheNodeCreateTime")
      Lude.<*> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "CacheNodeId")
      Lude.<*> (x Lude..@? "NodeGroupId")
      Lude.<*> (x Lude..@? "SnapshotCreateTime")
      Lude.<*> (x Lude..@? "CacheSize")
