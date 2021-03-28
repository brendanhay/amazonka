{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.NodeGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.NodeGroup
  ( NodeGroup (..)
  -- * Smart constructor
  , mkNodeGroup
  -- * Lenses
  , ngNodeGroupId
  , ngNodeGroupMembers
  , ngPrimaryEndpoint
  , ngReaderEndpoint
  , ngSlots
  , ngStatus
  ) where

import qualified Network.AWS.ElastiCache.Types.Endpoint as Types
import qualified Network.AWS.ElastiCache.Types.NodeGroupMember as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.
--
-- /See:/ 'mkNodeGroup' smart constructor.
data NodeGroup = NodeGroup'
  { nodeGroupId :: Core.Maybe Core.Text
    -- ^ The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group. 
  , nodeGroupMembers :: Core.Maybe [Types.NodeGroupMember]
    -- ^ A list containing information about individual nodes within the node group (shard).
  , primaryEndpoint :: Core.Maybe Types.Endpoint
    -- ^ The endpoint of the primary node in this node group (shard).
  , readerEndpoint :: Core.Maybe Types.Endpoint
    -- ^ The endpoint of the replica nodes in this node group (shard).
  , slots :: Core.Maybe Core.Text
    -- ^ The keyspace for this node group (shard).
  , status :: Core.Maybe Core.Text
    -- ^ The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeGroup' value with any optional fields omitted.
mkNodeGroup
    :: NodeGroup
mkNodeGroup
  = NodeGroup'{nodeGroupId = Core.Nothing,
               nodeGroupMembers = Core.Nothing, primaryEndpoint = Core.Nothing,
               readerEndpoint = Core.Nothing, slots = Core.Nothing,
               status = Core.Nothing}

-- | The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group. 
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNodeGroupId :: Lens.Lens' NodeGroup (Core.Maybe Core.Text)
ngNodeGroupId = Lens.field @"nodeGroupId"
{-# INLINEABLE ngNodeGroupId #-}
{-# DEPRECATED nodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead"  #-}

-- | A list containing information about individual nodes within the node group (shard).
--
-- /Note:/ Consider using 'nodeGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNodeGroupMembers :: Lens.Lens' NodeGroup (Core.Maybe [Types.NodeGroupMember])
ngNodeGroupMembers = Lens.field @"nodeGroupMembers"
{-# INLINEABLE ngNodeGroupMembers #-}
{-# DEPRECATED nodeGroupMembers "Use generic-lens or generic-optics with 'nodeGroupMembers' instead"  #-}

-- | The endpoint of the primary node in this node group (shard).
--
-- /Note:/ Consider using 'primaryEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngPrimaryEndpoint :: Lens.Lens' NodeGroup (Core.Maybe Types.Endpoint)
ngPrimaryEndpoint = Lens.field @"primaryEndpoint"
{-# INLINEABLE ngPrimaryEndpoint #-}
{-# DEPRECATED primaryEndpoint "Use generic-lens or generic-optics with 'primaryEndpoint' instead"  #-}

-- | The endpoint of the replica nodes in this node group (shard).
--
-- /Note:/ Consider using 'readerEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngReaderEndpoint :: Lens.Lens' NodeGroup (Core.Maybe Types.Endpoint)
ngReaderEndpoint = Lens.field @"readerEndpoint"
{-# INLINEABLE ngReaderEndpoint #-}
{-# DEPRECATED readerEndpoint "Use generic-lens or generic-optics with 'readerEndpoint' instead"  #-}

-- | The keyspace for this node group (shard).
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngSlots :: Lens.Lens' NodeGroup (Core.Maybe Core.Text)
ngSlots = Lens.field @"slots"
{-# INLINEABLE ngSlots #-}
{-# DEPRECATED slots "Use generic-lens or generic-optics with 'slots' instead"  #-}

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngStatus :: Lens.Lens' NodeGroup (Core.Maybe Core.Text)
ngStatus = Lens.field @"status"
{-# INLINEABLE ngStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML NodeGroup where
        parseXML x
          = NodeGroup' Core.<$>
              (x Core..@? "NodeGroupId") Core.<*>
                x Core..@? "NodeGroupMembers" Core..<@>
                  Core.parseXMLList "NodeGroupMember"
                Core.<*> x Core..@? "PrimaryEndpoint"
                Core.<*> x Core..@? "ReaderEndpoint"
                Core.<*> x Core..@? "Slots"
                Core.<*> x Core..@? "Status"
