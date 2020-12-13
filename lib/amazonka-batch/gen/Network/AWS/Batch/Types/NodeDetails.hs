{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeDetails
  ( NodeDetails (..),

    -- * Smart constructor
    mkNodeDetails,

    -- * Lenses
    ndNodeIndex,
    ndIsMainNode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing the details of a multi-node parallel job node.
--
-- /See:/ 'mkNodeDetails' smart constructor.
data NodeDetails = NodeDetails'
  { -- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
    nodeIndex :: Lude.Maybe Lude.Int,
    -- | Specifies whether the current node is the main node for a multi-node parallel job.
    isMainNode :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NodeDetails' with the minimum fields required to make a request.
--
-- * 'nodeIndex' - The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
-- * 'isMainNode' - Specifies whether the current node is the main node for a multi-node parallel job.
mkNodeDetails ::
  NodeDetails
mkNodeDetails =
  NodeDetails' {nodeIndex = Lude.Nothing, isMainNode = Lude.Nothing}

-- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
--
-- /Note:/ Consider using 'nodeIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndNodeIndex :: Lens.Lens' NodeDetails (Lude.Maybe Lude.Int)
ndNodeIndex = Lens.lens (nodeIndex :: NodeDetails -> Lude.Maybe Lude.Int) (\s a -> s {nodeIndex = a} :: NodeDetails)
{-# DEPRECATED ndNodeIndex "Use generic-lens or generic-optics with 'nodeIndex' instead." #-}

-- | Specifies whether the current node is the main node for a multi-node parallel job.
--
-- /Note:/ Consider using 'isMainNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndIsMainNode :: Lens.Lens' NodeDetails (Lude.Maybe Lude.Bool)
ndIsMainNode = Lens.lens (isMainNode :: NodeDetails -> Lude.Maybe Lude.Bool) (\s a -> s {isMainNode = a} :: NodeDetails)
{-# DEPRECATED ndIsMainNode "Use generic-lens or generic-optics with 'isMainNode' instead." #-}

instance Lude.FromJSON NodeDetails where
  parseJSON =
    Lude.withObject
      "NodeDetails"
      ( \x ->
          NodeDetails'
            Lude.<$> (x Lude..:? "nodeIndex") Lude.<*> (x Lude..:? "isMainNode")
      )
