-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Node
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.Node
  ( Node (..),

    -- * Smart constructor
    mkNode,

    -- * Lenses
    nNodeStatus,
    nParameterGroupStatus,
    nAvailabilityZone,
    nNodeId,
    nEndpoint,
    nNodeCreateTime,
  )
where

import Network.AWS.DAX.Types.Endpoint
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an individual node within a DAX cluster.
--
-- /See:/ 'mkNode' smart constructor.
data Node = Node'
  { nodeStatus :: Lude.Maybe Lude.Text,
    parameterGroupStatus :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Maybe Lude.Text,
    nodeId :: Lude.Maybe Lude.Text,
    endpoint :: Lude.Maybe Endpoint,
    nodeCreateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Node' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone (AZ) in which the node has been deployed.
-- * 'endpoint' - The endpoint for the node, consisting of a DNS name and a port number. Client applications can connect directly to a node endpoint, if desired (as an alternative to allowing DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
-- * 'nodeCreateTime' - The date and time (in UNIX epoch format) when the node was launched.
-- * 'nodeId' - A system-generated identifier for the node.
-- * 'nodeStatus' - The current status of the node. For example: @available@ .
-- * 'parameterGroupStatus' - The status of the parameter group associated with this node. For example, @in-sync@ .
mkNode ::
  Node
mkNode =
  Node'
    { nodeStatus = Lude.Nothing,
      parameterGroupStatus = Lude.Nothing,
      availabilityZone = Lude.Nothing,
      nodeId = Lude.Nothing,
      endpoint = Lude.Nothing,
      nodeCreateTime = Lude.Nothing
    }

-- | The current status of the node. For example: @available@ .
--
-- /Note:/ Consider using 'nodeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNodeStatus :: Lens.Lens' Node (Lude.Maybe Lude.Text)
nNodeStatus = Lens.lens (nodeStatus :: Node -> Lude.Maybe Lude.Text) (\s a -> s {nodeStatus = a} :: Node)
{-# DEPRECATED nNodeStatus "Use generic-lens or generic-optics with 'nodeStatus' instead." #-}

-- | The status of the parameter group associated with this node. For example, @in-sync@ .
--
-- /Note:/ Consider using 'parameterGroupStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nParameterGroupStatus :: Lens.Lens' Node (Lude.Maybe Lude.Text)
nParameterGroupStatus = Lens.lens (parameterGroupStatus :: Node -> Lude.Maybe Lude.Text) (\s a -> s {parameterGroupStatus = a} :: Node)
{-# DEPRECATED nParameterGroupStatus "Use generic-lens or generic-optics with 'parameterGroupStatus' instead." #-}

-- | The Availability Zone (AZ) in which the node has been deployed.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nAvailabilityZone :: Lens.Lens' Node (Lude.Maybe Lude.Text)
nAvailabilityZone = Lens.lens (availabilityZone :: Node -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: Node)
{-# DEPRECATED nAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A system-generated identifier for the node.
--
-- /Note:/ Consider using 'nodeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNodeId :: Lens.Lens' Node (Lude.Maybe Lude.Text)
nNodeId = Lens.lens (nodeId :: Node -> Lude.Maybe Lude.Text) (\s a -> s {nodeId = a} :: Node)
{-# DEPRECATED nNodeId "Use generic-lens or generic-optics with 'nodeId' instead." #-}

-- | The endpoint for the node, consisting of a DNS name and a port number. Client applications can connect directly to a node endpoint, if desired (as an alternative to allowing DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nEndpoint :: Lens.Lens' Node (Lude.Maybe Endpoint)
nEndpoint = Lens.lens (endpoint :: Node -> Lude.Maybe Endpoint) (\s a -> s {endpoint = a} :: Node)
{-# DEPRECATED nEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The date and time (in UNIX epoch format) when the node was launched.
--
-- /Note:/ Consider using 'nodeCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nNodeCreateTime :: Lens.Lens' Node (Lude.Maybe Lude.Timestamp)
nNodeCreateTime = Lens.lens (nodeCreateTime :: Node -> Lude.Maybe Lude.Timestamp) (\s a -> s {nodeCreateTime = a} :: Node)
{-# DEPRECATED nNodeCreateTime "Use generic-lens or generic-optics with 'nodeCreateTime' instead." #-}

instance Lude.FromJSON Node where
  parseJSON =
    Lude.withObject
      "Node"
      ( \x ->
          Node'
            Lude.<$> (x Lude..:? "NodeStatus")
            Lude.<*> (x Lude..:? "ParameterGroupStatus")
            Lude.<*> (x Lude..:? "AvailabilityZone")
            Lude.<*> (x Lude..:? "NodeId")
            Lude.<*> (x Lude..:? "Endpoint")
            Lude.<*> (x Lude..:? "NodeCreateTime")
      )
