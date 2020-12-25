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
    ndIsMainNode,
    ndNodeIndex,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the details of a multi-node parallel job node.
--
-- /See:/ 'mkNodeDetails' smart constructor.
data NodeDetails = NodeDetails'
  { -- | Specifies whether the current node is the main node for a multi-node parallel job.
    isMainNode :: Core.Maybe Core.Bool,
    -- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
    nodeIndex :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeDetails' value with any optional fields omitted.
mkNodeDetails ::
  NodeDetails
mkNodeDetails =
  NodeDetails' {isMainNode = Core.Nothing, nodeIndex = Core.Nothing}

-- | Specifies whether the current node is the main node for a multi-node parallel job.
--
-- /Note:/ Consider using 'isMainNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndIsMainNode :: Lens.Lens' NodeDetails (Core.Maybe Core.Bool)
ndIsMainNode = Lens.field @"isMainNode"
{-# DEPRECATED ndIsMainNode "Use generic-lens or generic-optics with 'isMainNode' instead." #-}

-- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
--
-- /Note:/ Consider using 'nodeIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ndNodeIndex :: Lens.Lens' NodeDetails (Core.Maybe Core.Int)
ndNodeIndex = Lens.field @"nodeIndex"
{-# DEPRECATED ndNodeIndex "Use generic-lens or generic-optics with 'nodeIndex' instead." #-}

instance Core.FromJSON NodeDetails where
  parseJSON =
    Core.withObject "NodeDetails" Core.$
      \x ->
        NodeDetails'
          Core.<$> (x Core..:? "isMainNode") Core.<*> (x Core..:? "nodeIndex")
