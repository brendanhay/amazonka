-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroupMemberUpdateStatus
  ( NodeGroupMemberUpdateStatus (..),

    -- * Smart constructor
    mkNodeGroupMemberUpdateStatus,

    -- * Lenses
    ngmusNodeUpdateEndDate,
    ngmusNodeUpdateInitiatedBy,
    ngmusNodeUpdateStatusModifiedDate,
    ngmusCacheClusterId,
    ngmusCacheNodeId,
    ngmusNodeUpdateInitiatedDate,
    ngmusNodeUpdateStartDate,
    ngmusNodeUpdateStatus,
    ngmusNodeDeletionDate,
  )
where

import Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy
import Network.AWS.ElastiCache.Types.NodeUpdateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status of the service update on the node group member
--
-- /See:/ 'mkNodeGroupMemberUpdateStatus' smart constructor.
data NodeGroupMemberUpdateStatus = NodeGroupMemberUpdateStatus'
  { nodeUpdateEndDate ::
      Lude.Maybe Lude.ISO8601,
    nodeUpdateInitiatedBy ::
      Lude.Maybe NodeUpdateInitiatedBy,
    nodeUpdateStatusModifiedDate ::
      Lude.Maybe Lude.ISO8601,
    cacheClusterId ::
      Lude.Maybe Lude.Text,
    cacheNodeId :: Lude.Maybe Lude.Text,
    nodeUpdateInitiatedDate ::
      Lude.Maybe Lude.ISO8601,
    nodeUpdateStartDate ::
      Lude.Maybe Lude.ISO8601,
    nodeUpdateStatus ::
      Lude.Maybe NodeUpdateStatus,
    nodeDeletionDate ::
      Lude.Maybe Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeGroupMemberUpdateStatus' with the minimum fields required to make a request.
--
-- * 'cacheClusterId' - The cache cluster ID
-- * 'cacheNodeId' - The node ID of the cache cluster
-- * 'nodeDeletionDate' - The deletion date of the node
-- * 'nodeUpdateEndDate' - The end date of the update for a node
-- * 'nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or automatically applied
-- * 'nodeUpdateInitiatedDate' - The date when the update is triggered
-- * 'nodeUpdateStartDate' - The start date of the update for a node
-- * 'nodeUpdateStatus' - The update status of the node
-- * 'nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified
mkNodeGroupMemberUpdateStatus ::
  NodeGroupMemberUpdateStatus
mkNodeGroupMemberUpdateStatus =
  NodeGroupMemberUpdateStatus'
    { nodeUpdateEndDate = Lude.Nothing,
      nodeUpdateInitiatedBy = Lude.Nothing,
      nodeUpdateStatusModifiedDate = Lude.Nothing,
      cacheClusterId = Lude.Nothing,
      cacheNodeId = Lude.Nothing,
      nodeUpdateInitiatedDate = Lude.Nothing,
      nodeUpdateStartDate = Lude.Nothing,
      nodeUpdateStatus = Lude.Nothing,
      nodeDeletionDate = Lude.Nothing
    }

-- | The end date of the update for a node
--
-- /Note:/ Consider using 'nodeUpdateEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateEndDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe Lude.ISO8601)
ngmusNodeUpdateEndDate = Lens.lens (nodeUpdateEndDate :: NodeGroupMemberUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateEndDate = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusNodeUpdateEndDate "Use generic-lens or generic-optics with 'nodeUpdateEndDate' instead." #-}

-- | Reflects whether the update was initiated by the customer or automatically applied
--
-- /Note:/ Consider using 'nodeUpdateInitiatedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateInitiatedBy :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe NodeUpdateInitiatedBy)
ngmusNodeUpdateInitiatedBy = Lens.lens (nodeUpdateInitiatedBy :: NodeGroupMemberUpdateStatus -> Lude.Maybe NodeUpdateInitiatedBy) (\s a -> s {nodeUpdateInitiatedBy = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusNodeUpdateInitiatedBy "Use generic-lens or generic-optics with 'nodeUpdateInitiatedBy' instead." #-}

-- | The date when the NodeUpdateStatus was last modified
--
-- /Note:/ Consider using 'nodeUpdateStatusModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateStatusModifiedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe Lude.ISO8601)
ngmusNodeUpdateStatusModifiedDate = Lens.lens (nodeUpdateStatusModifiedDate :: NodeGroupMemberUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateStatusModifiedDate = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusNodeUpdateStatusModifiedDate "Use generic-lens or generic-optics with 'nodeUpdateStatusModifiedDate' instead." #-}

-- | The cache cluster ID
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusCacheClusterId :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe Lude.Text)
ngmusCacheClusterId = Lens.lens (cacheClusterId :: NodeGroupMemberUpdateStatus -> Lude.Maybe Lude.Text) (\s a -> s {cacheClusterId = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The node ID of the cache cluster
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusCacheNodeId :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe Lude.Text)
ngmusCacheNodeId = Lens.lens (cacheNodeId :: NodeGroupMemberUpdateStatus -> Lude.Maybe Lude.Text) (\s a -> s {cacheNodeId = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | The date when the update is triggered
--
-- /Note:/ Consider using 'nodeUpdateInitiatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateInitiatedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe Lude.ISO8601)
ngmusNodeUpdateInitiatedDate = Lens.lens (nodeUpdateInitiatedDate :: NodeGroupMemberUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateInitiatedDate = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusNodeUpdateInitiatedDate "Use generic-lens or generic-optics with 'nodeUpdateInitiatedDate' instead." #-}

-- | The start date of the update for a node
--
-- /Note:/ Consider using 'nodeUpdateStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateStartDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe Lude.ISO8601)
ngmusNodeUpdateStartDate = Lens.lens (nodeUpdateStartDate :: NodeGroupMemberUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeUpdateStartDate = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusNodeUpdateStartDate "Use generic-lens or generic-optics with 'nodeUpdateStartDate' instead." #-}

-- | The update status of the node
--
-- /Note:/ Consider using 'nodeUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateStatus :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe NodeUpdateStatus)
ngmusNodeUpdateStatus = Lens.lens (nodeUpdateStatus :: NodeGroupMemberUpdateStatus -> Lude.Maybe NodeUpdateStatus) (\s a -> s {nodeUpdateStatus = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusNodeUpdateStatus "Use generic-lens or generic-optics with 'nodeUpdateStatus' instead." #-}

-- | The deletion date of the node
--
-- /Note:/ Consider using 'nodeDeletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeDeletionDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Lude.Maybe Lude.ISO8601)
ngmusNodeDeletionDate = Lens.lens (nodeDeletionDate :: NodeGroupMemberUpdateStatus -> Lude.Maybe Lude.ISO8601) (\s a -> s {nodeDeletionDate = a} :: NodeGroupMemberUpdateStatus)
{-# DEPRECATED ngmusNodeDeletionDate "Use generic-lens or generic-optics with 'nodeDeletionDate' instead." #-}

instance Lude.FromXML NodeGroupMemberUpdateStatus where
  parseXML x =
    NodeGroupMemberUpdateStatus'
      Lude.<$> (x Lude..@? "NodeUpdateEndDate")
      Lude.<*> (x Lude..@? "NodeUpdateInitiatedBy")
      Lude.<*> (x Lude..@? "NodeUpdateStatusModifiedDate")
      Lude.<*> (x Lude..@? "CacheClusterId")
      Lude.<*> (x Lude..@? "CacheNodeId")
      Lude.<*> (x Lude..@? "NodeUpdateInitiatedDate")
      Lude.<*> (x Lude..@? "NodeUpdateStartDate")
      Lude.<*> (x Lude..@? "NodeUpdateStatus")
      Lude.<*> (x Lude..@? "NodeDeletionDate")
