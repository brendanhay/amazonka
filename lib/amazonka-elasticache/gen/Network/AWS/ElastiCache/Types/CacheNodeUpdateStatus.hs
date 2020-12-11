-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheNodeUpdateStatus
  ( CacheNodeUpdateStatus (..),

    -- * Smart constructor
    mkCacheNodeUpdateStatus,

    -- * Lenses
    cnusNodeUpdateEndDate,
    cnusNodeUpdateInitiatedBy,
    cnusNodeUpdateStatusModifiedDate,
    cnusCacheNodeId,
    cnusNodeUpdateInitiatedDate,
    cnusNodeUpdateStartDate,
    cnusNodeUpdateStatus,
    cnusNodeDeletionDate,
  )
where

import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the service update on the cache node
--
-- /See:/ 'mkCacheNodeUpdateStatus' smart constructor.
data CacheNodeUpdateStatus = CacheNodeUpdateStatus'
  { nodeUpdateEndDate ::
      Lude.Maybe Lude.ISO8601,
    nodeUpdateInitiatedBy ::
      Lude.Maybe NodeUpdateInitiatedBy,
    nodeUpdateStatusModifiedDate ::
      Lude.Maybe Lude.ISO8601,
    cacheNodeId :: Lude.Maybe Lude.Text,
    nodeUpdateInitiatedDate ::
      Lude.Maybe Lude.ISO8601,
    nodeUpdateStartDate :: Lude.Maybe Lude.ISO8601,
    nodeUpdateStatus :: Lude.Maybe NodeUpdateStatus,
    nodeDeletionDate :: Lude.Maybe Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheNodeUpdateStatus' with the minimum fields required to make a request.
--
-- * 'cacheNodeId' - The node ID of the cache cluster
-- * 'nodeDeletionDate' - The deletion date of the node
-- * 'nodeUpdateEndDate' - The end date of the update for a node
-- * 'nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or automatically applied
-- * 'nodeUpdateInitiatedDate' - The date when the update is triggered
-- * 'nodeUpdateStartDate' - The start date of the update for a node
-- * 'nodeUpdateStatus' - The update status of the node
-- * 'nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified>
mkCacheNodeUpdateStatus ::
  CacheNodeUpdateStatus
mkCacheNodeUpdateStatus =
  CacheNodeUpdateStatus'
    { nodeUpdateEndDate = Lude.Nothing,
      nodeUpdateInitiatedBy = Lude.Nothing,
      nodeUpdateStatusModifiedDate = Lude.Nothing,
      cacheNodeId = Lude.Nothing,
      nodeUpdateInitiatedDate = Lude.Nothing,
      nodeUpdateStartDate = Lude.Nothing,
      nodeUpdateStatus = Lude.Nothing,
      nodeDeletionDate = Lude.Nothing
    }

-- | The end date of the update for a node
--
-- /Note:/ Consider using 'nodeUpdateEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusNodeUpdateEndDate :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe Lude.ISO8601)
cnusNodeUpdateEndDate = Lens.lens (nodeUpdateEndDate :: CacheNodeUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateEndDate = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusNodeUpdateEndDate "Use generic-lens or generic-optics with 'nodeUpdateEndDate' instead." #-}

-- | Reflects whether the update was initiated by the customer or automatically applied
--
-- /Note:/ Consider using 'nodeUpdateInitiatedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusNodeUpdateInitiatedBy :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe NodeUpdateInitiatedBy)
cnusNodeUpdateInitiatedBy = Lens.lens (nodeUpdateInitiatedBy :: CacheNodeUpdateStatus -> Lude.Maybe NodeUpdateInitiatedBy) (\s a -> s {nodeUpdateInitiatedBy = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusNodeUpdateInitiatedBy "Use generic-lens or generic-optics with 'nodeUpdateInitiatedBy' instead." #-}

-- | The date when the NodeUpdateStatus was last modified>
--
-- /Note:/ Consider using 'nodeUpdateStatusModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusNodeUpdateStatusModifiedDate :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe Lude.ISO8601)
cnusNodeUpdateStatusModifiedDate = Lens.lens (nodeUpdateStatusModifiedDate :: CacheNodeUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateStatusModifiedDate = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusNodeUpdateStatusModifiedDate "Use generic-lens or generic-optics with 'nodeUpdateStatusModifiedDate' instead." #-}

-- | The node ID of the cache cluster
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusCacheNodeId :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe Lude.Text)
cnusCacheNodeId = Lens.lens (cacheNodeId :: CacheNodeUpdateStatus -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeId = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | The date when the update is triggered
--
-- /Note:/ Consider using 'nodeUpdateInitiatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusNodeUpdateInitiatedDate :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe Lude.ISO8601)
cnusNodeUpdateInitiatedDate = Lens.lens (nodeUpdateInitiatedDate :: CacheNodeUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateInitiatedDate = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusNodeUpdateInitiatedDate "Use generic-lens or generic-optics with 'nodeUpdateInitiatedDate' instead." #-}

-- | The start date of the update for a node
--
-- /Note:/ Consider using 'nodeUpdateStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusNodeUpdateStartDate :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe Lude.ISO8601)
cnusNodeUpdateStartDate = Lens.lens (nodeUpdateStartDate :: CacheNodeUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateStartDate = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusNodeUpdateStartDate "Use generic-lens or generic-optics with 'nodeUpdateStartDate' instead." #-}

-- | The update status of the node
--
-- /Note:/ Consider using 'nodeUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusNodeUpdateStatus :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe NodeUpdateStatus)
cnusNodeUpdateStatus = Lens.lens (nodeUpdateStatus :: CacheNodeUpdateStatus -> Lude.Maybe NodeUpdateStatus) (\s a -> s {nodeUpdateStatus = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusNodeUpdateStatus "Use generic-lens or generic-optics with 'nodeUpdateStatus' instead." #-}

-- | The deletion date of the node
--
-- /Note:/ Consider using 'nodeDeletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnusNodeDeletionDate :: Lens.Lens' CacheNodeUpdateStatus (Lude.Maybe Lude.ISO8601)
cnusNodeDeletionDate = Lens.lens (nodeDeletionDate :: CacheNodeUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeDeletionDate = a} :: CacheNodeUpdateStatus)
{-# DEPRECATED cnusNodeDeletionDate "Use generic-lens or generic-optics with 'nodeDeletionDate' instead." #-}

instance Lude.FromXML CacheNodeUpdateStatus where
  parseXML x =
    CacheNodeUpdateStatus'
      Lude.<$> (x Lude..@? "NodeUpdateEndDate")
      Lude.<*> (x Lude..@? "NodeUpdateInitiatedBy")
      Lude.<*> (x Lude..@? "NodeUpdateStatusModifiedDate")
      Lude.<*> (x Lude..@? "CacheNodeId")
      Lude.<*> (x Lude..@? "NodeUpdateInitiatedDate")
      Lude.<*> (x Lude..@? "NodeUpdateStartDate")
      Lude.<*> (x Lude..@? "NodeUpdateStatus")
      Lude.<*> (x Lude..@? "NodeDeletionDate")
