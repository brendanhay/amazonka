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
    ngStatus,
    ngPrimaryEndpoint,
    ngSlots,
    ngNodeGroupMembers,
    ngNodeGroupId,
    ngReaderEndpoint,
  )
where

import Network.AWS.ElastiCache.Types.Endpoint
import Network.AWS.ElastiCache.Types.NodeGroupMember
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.
--
-- /See:/ 'mkNodeGroup' smart constructor.
data NodeGroup = NodeGroup'
  { status :: Lude.Maybe Lude.Text,
    primaryEndpoint :: Lude.Maybe Endpoint,
    slots :: Lude.Maybe Lude.Text,
    nodeGroupMembers :: Lude.Maybe [NodeGroupMember],
    nodeGroupId :: Lude.Maybe Lude.Text,
    readerEndpoint :: Lude.Maybe Endpoint
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeGroup' with the minimum fields required to make a request.
--
-- * 'nodeGroupId' - The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group.
-- * 'nodeGroupMembers' - A list containing information about individual nodes within the node group (shard).
-- * 'primaryEndpoint' - The endpoint of the primary node in this node group (shard).
-- * 'readerEndpoint' - The endpoint of the replica nodes in this node group (shard).
-- * 'slots' - The keyspace for this node group (shard).
-- * 'status' - The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
mkNodeGroup ::
  NodeGroup
mkNodeGroup =
  NodeGroup'
    { status = Lude.Nothing,
      primaryEndpoint = Lude.Nothing,
      slots = Lude.Nothing,
      nodeGroupMembers = Lude.Nothing,
      nodeGroupId = Lude.Nothing,
      readerEndpoint = Lude.Nothing
    }

-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngStatus :: Lens.Lens' NodeGroup (Lude.Maybe Lude.Text)
ngStatus = Lens.lens (status :: NodeGroup -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: NodeGroup)
{-# DEPRECATED ngStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The endpoint of the primary node in this node group (shard).
--
-- /Note:/ Consider using 'primaryEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngPrimaryEndpoint :: Lens.Lens' NodeGroup (Lude.Maybe Endpoint)
ngPrimaryEndpoint = Lens.lens (primaryEndpoint :: NodeGroup -> Lude.Maybe Endpoint) (\s a -> s {primaryEndpoint = a} :: NodeGroup)
{-# DEPRECATED ngPrimaryEndpoint "Use generic-lens or generic-optics with 'primaryEndpoint' instead." #-}

-- | The keyspace for this node group (shard).
--
-- /Note:/ Consider using 'slots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngSlots :: Lens.Lens' NodeGroup (Lude.Maybe Lude.Text)
ngSlots = Lens.lens (slots :: NodeGroup -> Lude.Maybe Lude.Text) (\s a -> s {slots = a} :: NodeGroup)
{-# DEPRECATED ngSlots "Use generic-lens or generic-optics with 'slots' instead." #-}

-- | A list containing information about individual nodes within the node group (shard).
--
-- /Note:/ Consider using 'nodeGroupMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNodeGroupMembers :: Lens.Lens' NodeGroup (Lude.Maybe [NodeGroupMember])
ngNodeGroupMembers = Lens.lens (nodeGroupMembers :: NodeGroup -> Lude.Maybe [NodeGroupMember]) (\s a -> s {nodeGroupMembers = a} :: NodeGroup)
{-# DEPRECATED ngNodeGroupMembers "Use generic-lens or generic-optics with 'nodeGroupMembers' instead." #-}

-- | The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user can provide the id for a node group.
--
-- /Note:/ Consider using 'nodeGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngNodeGroupId :: Lens.Lens' NodeGroup (Lude.Maybe Lude.Text)
ngNodeGroupId = Lens.lens (nodeGroupId :: NodeGroup -> Lude.Maybe Lude.Text) (\s a -> s {nodeGroupId = a} :: NodeGroup)
{-# DEPRECATED ngNodeGroupId "Use generic-lens or generic-optics with 'nodeGroupId' instead." #-}

-- | The endpoint of the replica nodes in this node group (shard).
--
-- /Note:/ Consider using 'readerEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ngReaderEndpoint :: Lens.Lens' NodeGroup (Lude.Maybe Endpoint)
ngReaderEndpoint = Lens.lens (readerEndpoint :: NodeGroup -> Lude.Maybe Endpoint) (\s a -> s {readerEndpoint = a} :: NodeGroup)
{-# DEPRECATED ngReaderEndpoint "Use generic-lens or generic-optics with 'readerEndpoint' instead." #-}

instance Lude.FromXML NodeGroup where
  parseXML x =
    NodeGroup'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "PrimaryEndpoint")
      Lude.<*> (x Lude..@? "Slots")
      Lude.<*> ( x Lude..@? "NodeGroupMembers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "NodeGroupMember")
               )
      Lude.<*> (x Lude..@? "NodeGroupId")
      Lude.<*> (x Lude..@? "ReaderEndpoint")
