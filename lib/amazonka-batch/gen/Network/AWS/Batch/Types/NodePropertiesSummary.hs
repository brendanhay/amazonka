{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    npsIsMainNode,
    npsNodeIndex,
    npsNumNodes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the properties of a node that is associated with a multi-node parallel job.
--
-- /See:/ 'mkNodePropertiesSummary' smart constructor.
data NodePropertiesSummary = NodePropertiesSummary'
  { -- | Specifies whether the current node is the main node for a multi-node parallel job.
    isMainNode :: Core.Maybe Core.Bool,
    -- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
    nodeIndex :: Core.Maybe Core.Int,
    -- | The number of nodes associated with a multi-node parallel job.
    numNodes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodePropertiesSummary' value with any optional fields omitted.
mkNodePropertiesSummary ::
  NodePropertiesSummary
mkNodePropertiesSummary =
  NodePropertiesSummary'
    { isMainNode = Core.Nothing,
      nodeIndex = Core.Nothing,
      numNodes = Core.Nothing
    }

-- | Specifies whether the current node is the main node for a multi-node parallel job.
--
-- /Note:/ Consider using 'isMainNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npsIsMainNode :: Lens.Lens' NodePropertiesSummary (Core.Maybe Core.Bool)
npsIsMainNode = Lens.field @"isMainNode"
{-# DEPRECATED npsIsMainNode "Use generic-lens or generic-optics with 'isMainNode' instead." #-}

-- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
--
-- /Note:/ Consider using 'nodeIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npsNodeIndex :: Lens.Lens' NodePropertiesSummary (Core.Maybe Core.Int)
npsNodeIndex = Lens.field @"nodeIndex"
{-# DEPRECATED npsNodeIndex "Use generic-lens or generic-optics with 'nodeIndex' instead." #-}

-- | The number of nodes associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'numNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npsNumNodes :: Lens.Lens' NodePropertiesSummary (Core.Maybe Core.Int)
npsNumNodes = Lens.field @"numNodes"
{-# DEPRECATED npsNumNodes "Use generic-lens or generic-optics with 'numNodes' instead." #-}

instance Core.FromJSON NodePropertiesSummary where
  parseJSON =
    Core.withObject "NodePropertiesSummary" Core.$
      \x ->
        NodePropertiesSummary'
          Core.<$> (x Core..:? "isMainNode")
          Core.<*> (x Core..:? "nodeIndex")
          Core.<*> (x Core..:? "numNodes")
