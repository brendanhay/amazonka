-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodePropertiesSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodePropertiesSummary
  ( NodePropertiesSummary (..),

    -- * Smart constructor
    mkNodePropertiesSummary,

    -- * Lenses
    npsNumNodes,
    npsNodeIndex,
    npsIsMainNode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the properties of a node that is associated with a multi-node parallel job.
--
-- /See:/ 'mkNodePropertiesSummary' smart constructor.
data NodePropertiesSummary = NodePropertiesSummary'
  { numNodes ::
      Lude.Maybe Lude.Int,
    nodeIndex :: Lude.Maybe Lude.Int,
    isMainNode :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodePropertiesSummary' with the minimum fields required to make a request.
--
-- * 'isMainNode' - Specifies whether the current node is the main node for a multi-node parallel job.
-- * 'nodeIndex' - The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
-- * 'numNodes' - The number of nodes associated with a multi-node parallel job.
mkNodePropertiesSummary ::
  NodePropertiesSummary
mkNodePropertiesSummary =
  NodePropertiesSummary'
    { numNodes = Lude.Nothing,
      nodeIndex = Lude.Nothing,
      isMainNode = Lude.Nothing
    }

-- | The number of nodes associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'numNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npsNumNodes :: Lens.Lens' NodePropertiesSummary (Lude.Maybe Lude.Int)
npsNumNodes = Lens.lens (numNodes :: NodePropertiesSummary -> Lude.Maybe Lude.Int) (\s a -> s {numNodes = a} :: NodePropertiesSummary)
{-# DEPRECATED npsNumNodes "Use generic-lens or generic-optics with 'numNodes' instead." #-}

-- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
--
-- /Note:/ Consider using 'nodeIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npsNodeIndex :: Lens.Lens' NodePropertiesSummary (Lude.Maybe Lude.Int)
npsNodeIndex = Lens.lens (nodeIndex :: NodePropertiesSummary -> Lude.Maybe Lude.Int) (\s a -> s {nodeIndex = a} :: NodePropertiesSummary)
{-# DEPRECATED npsNodeIndex "Use generic-lens or generic-optics with 'nodeIndex' instead." #-}

-- | Specifies whether the current node is the main node for a multi-node parallel job.
--
-- /Note:/ Consider using 'isMainNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npsIsMainNode :: Lens.Lens' NodePropertiesSummary (Lude.Maybe Lude.Bool)
npsIsMainNode = Lens.lens (isMainNode :: NodePropertiesSummary -> Lude.Maybe Lude.Bool) (\s a -> s {isMainNode = a} :: NodePropertiesSummary)
{-# DEPRECATED npsIsMainNode "Use generic-lens or generic-optics with 'isMainNode' instead." #-}

instance Lude.FromJSON NodePropertiesSummary where
  parseJSON =
    Lude.withObject
      "NodePropertiesSummary"
      ( \x ->
          NodePropertiesSummary'
            Lude.<$> (x Lude..:? "numNodes")
            Lude.<*> (x Lude..:? "nodeIndex")
            Lude.<*> (x Lude..:? "isMainNode")
      )
