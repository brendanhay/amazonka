-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ResizeClusterMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ResizeClusterMessage
  ( ResizeClusterMessage (..),

    -- * Smart constructor
    mkResizeClusterMessage,

    -- * Lenses
    rcmNumberOfNodes,
    rcmClassic,
    rcmClusterType,
    rcmNodeType,
    rcmClusterIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a resize cluster operation. For example, a scheduled action to run the @ResizeCluster@ API operation.
--
-- /See:/ 'mkResizeClusterMessage' smart constructor.
data ResizeClusterMessage = ResizeClusterMessage'
  { numberOfNodes ::
      Lude.Maybe Lude.Int,
    classic :: Lude.Maybe Lude.Bool,
    clusterType :: Lude.Maybe Lude.Text,
    nodeType :: Lude.Maybe Lude.Text,
    clusterIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResizeClusterMessage' with the minimum fields required to make a request.
--
-- * 'classic' - A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
-- * 'clusterIdentifier' - The unique identifier for the cluster to resize.
-- * 'clusterType' - The new cluster type for the specified cluster.
-- * 'nodeType' - The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
-- * 'numberOfNodes' - The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
mkResizeClusterMessage ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  ResizeClusterMessage
mkResizeClusterMessage pClusterIdentifier_ =
  ResizeClusterMessage'
    { numberOfNodes = Lude.Nothing,
      classic = Lude.Nothing,
      clusterType = Lude.Nothing,
      nodeType = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The new number of nodes for the cluster. If not specified, the cluster's current number of nodes is used.
--
-- /Note:/ Consider using 'numberOfNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmNumberOfNodes :: Lens.Lens' ResizeClusterMessage (Lude.Maybe Lude.Int)
rcmNumberOfNodes = Lens.lens (numberOfNodes :: ResizeClusterMessage -> Lude.Maybe Lude.Int) (\s a -> s {numberOfNodes = a} :: ResizeClusterMessage)
{-# DEPRECATED rcmNumberOfNodes "Use generic-lens or generic-optics with 'numberOfNodes' instead." #-}

-- | A boolean value indicating whether the resize operation is using the classic resize process. If you don't provide this parameter or set the value to @false@ , the resize type is elastic.
--
-- /Note:/ Consider using 'classic' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmClassic :: Lens.Lens' ResizeClusterMessage (Lude.Maybe Lude.Bool)
rcmClassic = Lens.lens (classic :: ResizeClusterMessage -> Lude.Maybe Lude.Bool) (\s a -> s {classic = a} :: ResizeClusterMessage)
{-# DEPRECATED rcmClassic "Use generic-lens or generic-optics with 'classic' instead." #-}

-- | The new cluster type for the specified cluster.
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmClusterType :: Lens.Lens' ResizeClusterMessage (Lude.Maybe Lude.Text)
rcmClusterType = Lens.lens (clusterType :: ResizeClusterMessage -> Lude.Maybe Lude.Text) (\s a -> s {clusterType = a} :: ResizeClusterMessage)
{-# DEPRECATED rcmClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The new node type for the nodes you are adding. If not specified, the cluster's current node type is used.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmNodeType :: Lens.Lens' ResizeClusterMessage (Lude.Maybe Lude.Text)
rcmNodeType = Lens.lens (nodeType :: ResizeClusterMessage -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: ResizeClusterMessage)
{-# DEPRECATED rcmNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

-- | The unique identifier for the cluster to resize.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcmClusterIdentifier :: Lens.Lens' ResizeClusterMessage Lude.Text
rcmClusterIdentifier = Lens.lens (clusterIdentifier :: ResizeClusterMessage -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ResizeClusterMessage)
{-# DEPRECATED rcmClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Lude.FromXML ResizeClusterMessage where
  parseXML x =
    ResizeClusterMessage'
      Lude.<$> (x Lude..@? "NumberOfNodes")
      Lude.<*> (x Lude..@? "Classic")
      Lude.<*> (x Lude..@? "ClusterType")
      Lude.<*> (x Lude..@? "NodeType")
      Lude.<*> (x Lude..@ "ClusterIdentifier")

instance Lude.ToQuery ResizeClusterMessage where
  toQuery ResizeClusterMessage' {..} =
    Lude.mconcat
      [ "NumberOfNodes" Lude.=: numberOfNodes,
        "Classic" Lude.=: classic,
        "ClusterType" Lude.=: clusterType,
        "NodeType" Lude.=: nodeType,
        "ClusterIdentifier" Lude.=: clusterIdentifier
      ]
