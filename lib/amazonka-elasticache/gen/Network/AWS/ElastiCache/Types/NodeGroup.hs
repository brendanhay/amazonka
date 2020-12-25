{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.NodeGroup
  ( NodeGroup (..),

    -- * Smart constructor
    mkNodeGroup,

    -- * Lenses
    ngNodeGroupId,
    ngNodeGroupMembers,
    ngPrimaryEndpoint,
    ngReaderEndpoint,
    ngSlots,
    ngStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types.Endpoint as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroupId as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroupMember as Types
import qualified Network.AWS.ElastiCache.Types.Slots as Types
import qualified Network.AWS.ElastiCache.Types.Status as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.
--
-- /See:/ 'mkNodeGroup' smart constructor.
data NodeGroup = NodeGroup'
  { -- | The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group.
    nodeGroupId :: Core.Maybe Types.NodeGroupId,
    -- | A list containing information about individual nodes within the node group (shard).
    nodeGroupMembers :: Core.Maybe [Types.NodeGroupMember],
    -- | The endpoint of the primary node in this node group (shard).
    primaryEndpoint :: Core.Maybe Types.Endpoint,
    -- | The endpoint of the replica nodes in this node group (shard).
    readerEndpoint :: Core.Maybe Types.Endpoint,
    -- | The keyspace for this node group (shard).
    slots :: Core.Maybe Types.Slots,
    -- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
    status :: Core.Maybe Types.Status
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeGroup' value with any optional fields omitted.
mkNodeGroup ::
  NodeGroup
mkNodeGroup =
  NodeGroup'
    { nodeGroupId = Core.Nothing,
      nodeGroupMembers = Core.Nothing,
      primaryEndpoint = Core.Nothing,
      readerEndpoint = Core.Nothing,
      slots = Core.Nothing,
      status = Core.Nothing
    }

-- | The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNodeGroupId :: Lens.Lens' NodeGroup (Core.Maybe Types.NodeGroupId)
ngNodeGroupId = Lens.field @"nodeGroupId"
{-# DEPRECATED ngNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | A list containing information about individual nodes within the node group (shard).
--
-- /Note:/ Consider using 'nodeGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNodeGroupMembers :: Lens.Lens' NodeGroup (Core.Maybe [Types.NodeGroupMember])
ngNodeGroupMembers = Lens.field @"nodeGroupMembers"
{-# DEPRECATED ngNodeGroupMembers "Use generic-lens or generic-optics with 'nodeGroupMembers' instead." #-}

-- | The endpoint of the primary node in this node group (shard).
--
-- /Note:/ Consider using 'primaryEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngPrimaryEndpoint :: Lens.Lens' NodeGroup (Core.Maybe Types.Endpoint)
ngPrimaryEndpoint = Lens.field @"primaryEndpoint"
{-# DEPRECATED ngPrimaryEndpoint "Use generic-lens or generic-optics with 'primaryEndpoint' instead." #-}

-- | The endpoint of the replica nodes in this node group (shard).
--
-- /Note:/ Consider using 'readerEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngReaderEndpoint :: Lens.Lens' NodeGroup (Core.Maybe Types.Endpoint)
ngReaderEndpoint = Lens.field @"readerEndpoint"
{-# DEPRECATED ngReaderEndpoint "Use generic-lens or generic-optics with 'readerEndpoint' instead." #-}

-- | The keyspace for this node group (shard).
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngSlots :: Lens.Lens' NodeGroup (Core.Maybe Types.Slots)
ngSlots = Lens.field @"slots"
{-# DEPRECATED ngSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngStatus :: Lens.Lens' NodeGroup (Core.Maybe Types.Status)
ngStatus = Lens.field @"status"
{-# DEPRECATED ngStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML NodeGroup where
  parseXML x =
    NodeGroup'
      Core.<$> (x Core..@? "NodeGroupId")
      Core.<*> ( x Core..@? "NodeGroupMembers"
                   Core..<@> Core.parseXMLList "NodeGroupMember"
               )
      Core.<*> (x Core..@? "PrimaryEndpoint")
      Core.<*> (x Core..@? "ReaderEndpoint")
      Core.<*> (x Core..@? "Slots")
      Core.<*> (x Core..@? "Status")
