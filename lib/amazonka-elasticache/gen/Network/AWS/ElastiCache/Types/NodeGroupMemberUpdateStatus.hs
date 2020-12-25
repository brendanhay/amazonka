{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ngmusCacheClusterId,
    ngmusCacheNodeId,
    ngmusNodeDeletionDate,
    ngmusNodeUpdateEndDate,
    ngmusNodeUpdateInitiatedBy,
    ngmusNodeUpdateInitiatedDate,
    ngmusNodeUpdateStartDate,
    ngmusNodeUpdateStatus,
    ngmusNodeUpdateStatusModifiedDate,
  )
where

import qualified Network.AWS.ElastiCache.Types.CacheClusterId as Types
import qualified Network.AWS.ElastiCache.Types.CacheNodeId as Types
import qualified Network.AWS.ElastiCache.Types.NodeUpdateInitiatedBy as Types
import qualified Network.AWS.ElastiCache.Types.NodeUpdateStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The status of the service update on the node group member
--
-- /See:/ 'mkNodeGroupMemberUpdateStatus' smart constructor.
data NodeGroupMemberUpdateStatus = NodeGroupMemberUpdateStatus'
  { -- | The cache cluster ID
    cacheClusterId :: Core.Maybe Types.CacheClusterId,
    -- | The node ID of the cache cluster
    cacheNodeId :: Core.Maybe Types.CacheNodeId,
    -- | The deletion date of the node
    nodeDeletionDate :: Core.Maybe Core.UTCTime,
    -- | The end date of the update for a node
    nodeUpdateEndDate :: Core.Maybe Core.UTCTime,
    -- | Reflects whether the update was initiated by the customer or automatically applied
    nodeUpdateInitiatedBy :: Core.Maybe Types.NodeUpdateInitiatedBy,
    -- | The date when the update is triggered
    nodeUpdateInitiatedDate :: Core.Maybe Core.UTCTime,
    -- | The start date of the update for a node
    nodeUpdateStartDate :: Core.Maybe Core.UTCTime,
    -- | The update status of the node
    nodeUpdateStatus :: Core.Maybe Types.NodeUpdateStatus,
    -- | The date when the NodeUpdateStatus was last modified
    nodeUpdateStatusModifiedDate :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NodeGroupMemberUpdateStatus' value with any optional fields omitted.
mkNodeGroupMemberUpdateStatus ::
  NodeGroupMemberUpdateStatus
mkNodeGroupMemberUpdateStatus =
  NodeGroupMemberUpdateStatus'
    { cacheClusterId = Core.Nothing,
      cacheNodeId = Core.Nothing,
      nodeDeletionDate = Core.Nothing,
      nodeUpdateEndDate = Core.Nothing,
      nodeUpdateInitiatedBy = Core.Nothing,
      nodeUpdateInitiatedDate = Core.Nothing,
      nodeUpdateStartDate = Core.Nothing,
      nodeUpdateStatus = Core.Nothing,
      nodeUpdateStatusModifiedDate = Core.Nothing
    }

-- | The cache cluster ID
--
-- /Note:/ Consider using 'cacheClusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusCacheClusterId :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Types.CacheClusterId)
ngmusCacheClusterId = Lens.field @"cacheClusterId"
{-# DEPRECATED ngmusCacheClusterId "Use generic-lens or generic-optics with 'cacheClusterId' instead." #-}

-- | The node ID of the cache cluster
--
-- /Note:/ Consider using 'cacheNodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusCacheNodeId :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Types.CacheNodeId)
ngmusCacheNodeId = Lens.field @"cacheNodeId"
{-# DEPRECATED ngmusCacheNodeId "Use generic-lens or generic-optics with 'cacheNodeId' instead." #-}

-- | The deletion date of the node
--
-- /Note:/ Consider using 'nodeDeletionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeDeletionDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
ngmusNodeDeletionDate = Lens.field @"nodeDeletionDate"
{-# DEPRECATED ngmusNodeDeletionDate "Use generic-lens or generic-optics with 'nodeDeletionDate' instead." #-}

-- | The end date of the update for a node
--
-- /Note:/ Consider using 'nodeUpdateEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateEndDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
ngmusNodeUpdateEndDate = Lens.field @"nodeUpdateEndDate"
{-# DEPRECATED ngmusNodeUpdateEndDate "Use generic-lens or generic-optics with 'nodeUpdateEndDate' instead." #-}

-- | Reflects whether the update was initiated by the customer or automatically applied
--
-- /Note:/ Consider using 'nodeUpdateInitiatedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateInitiatedBy :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Types.NodeUpdateInitiatedBy)
ngmusNodeUpdateInitiatedBy = Lens.field @"nodeUpdateInitiatedBy"
{-# DEPRECATED ngmusNodeUpdateInitiatedBy "Use generic-lens or generic-optics with 'nodeUpdateInitiatedBy' instead." #-}

-- | The date when the update is triggered
--
-- /Note:/ Consider using 'nodeUpdateInitiatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateInitiatedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
ngmusNodeUpdateInitiatedDate = Lens.field @"nodeUpdateInitiatedDate"
{-# DEPRECATED ngmusNodeUpdateInitiatedDate "Use generic-lens or generic-optics with 'nodeUpdateInitiatedDate' instead." #-}

-- | The start date of the update for a node
--
-- /Note:/ Consider using 'nodeUpdateStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateStartDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
ngmusNodeUpdateStartDate = Lens.field @"nodeUpdateStartDate"
{-# DEPRECATED ngmusNodeUpdateStartDate "Use generic-lens or generic-optics with 'nodeUpdateStartDate' instead." #-}

-- | The update status of the node
--
-- /Note:/ Consider using 'nodeUpdateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateStatus :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Types.NodeUpdateStatus)
ngmusNodeUpdateStatus = Lens.field @"nodeUpdateStatus"
{-# DEPRECATED ngmusNodeUpdateStatus "Use generic-lens or generic-optics with 'nodeUpdateStatus' instead." #-}

-- | The date when the NodeUpdateStatus was last modified
--
-- /Note:/ Consider using 'nodeUpdateStatusModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngmusNodeUpdateStatusModifiedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Core.Maybe Core.UTCTime)
ngmusNodeUpdateStatusModifiedDate = Lens.field @"nodeUpdateStatusModifiedDate"
{-# DEPRECATED ngmusNodeUpdateStatusModifiedDate "Use generic-lens or generic-optics with 'nodeUpdateStatusModifiedDate' instead." #-}

instance Core.FromXML NodeGroupMemberUpdateStatus where
  parseXML x =
    NodeGroupMemberUpdateStatus'
      Core.<$> (x Core..@? "CacheClusterId")
      Core.<*> (x Core..@? "CacheNodeId")
      Core.<*> (x Core..@? "NodeDeletionDate")
      Core.<*> (x Core..@? "NodeUpdateEndDate")
      Core.<*> (x Core..@? "NodeUpdateInitiatedBy")
      Core.<*> (x Core..@? "NodeUpdateInitiatedDate")
      Core.<*> (x Core..@? "NodeUpdateStartDate")
      Core.<*> (x Core..@? "NodeUpdateStatus")
      Core.<*> (x Core..@? "NodeUpdateStatusModifiedDate")
