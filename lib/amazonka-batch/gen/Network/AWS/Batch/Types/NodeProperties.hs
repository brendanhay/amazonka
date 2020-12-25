{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeProperties
  ( NodeProperties (..),

    -- * Smart constructor
    mkNodeProperties,

    -- * Lenses
    npNumNodes,
    npMainNode,
    npNodeRangeProperties,
  )
where

import qualified Network.AWS.Batch.Types.NodeRangeProperty as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the node properties of a multi-node parallel job.
--
-- /See:/ 'mkNodeProperties' smart constructor.
data NodeProperties = NodeProperties'
  { -- | The number of nodes associated with a multi-node parallel job.
    numNodes :: Core.Int,
    -- | Specifies the node index for the main node of a multi-node parallel job. This node index value must be fewer than the number of nodes.
    mainNode :: Core.Int,
    -- | A list of node ranges and their properties associated with a multi-node parallel job.
    nodeRangeProperties :: [Types.NodeRangeProperty]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeProperties' value with any optional fields omitted.
mkNodeProperties ::
  -- | 'numNodes'
  Core.Int ->
  -- | 'mainNode'
  Core.Int ->
  NodeProperties
mkNodeProperties numNodes mainNode =
  NodeProperties'
    { numNodes,
      mainNode,
      nodeRangeProperties = Core.mempty
    }

-- | The number of nodes associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'numNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNumNodes :: Lens.Lens' NodeProperties Core.Int
npNumNodes = Lens.field @"numNodes"
{-# DEPRECATED npNumNodes "Use generic-lens or generic-optics with 'numNodes' instead." #-}

-- | Specifies the node index for the main node of a multi-node parallel job. This node index value must be fewer than the number of nodes.
--
-- /Note:/ Consider using 'mainNode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npMainNode :: Lens.Lens' NodeProperties Core.Int
npMainNode = Lens.field @"mainNode"
{-# DEPRECATED npMainNode "Use generic-lens or generic-optics with 'mainNode' instead." #-}

-- | A list of node ranges and their properties associated with a multi-node parallel job.
--
-- /Note:/ Consider using 'nodeRangeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npNodeRangeProperties :: Lens.Lens' NodeProperties [Types.NodeRangeProperty]
npNodeRangeProperties = Lens.field @"nodeRangeProperties"
{-# DEPRECATED npNodeRangeProperties "Use generic-lens or generic-optics with 'nodeRangeProperties' instead." #-}

instance Core.FromJSON NodeProperties where
  toJSON NodeProperties {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("numNodes" Core..= numNodes),
            Core.Just ("mainNode" Core..= mainNode),
            Core.Just ("nodeRangeProperties" Core..= nodeRangeProperties)
          ]
      )

instance Core.FromJSON NodeProperties where
  parseJSON =
    Core.withObject "NodeProperties" Core.$
      \x ->
        NodeProperties'
          Core.<$> (x Core..: "numNodes")
          Core.<*> (x Core..: "mainNode")
          Core.<*> (x Core..:? "nodeRangeProperties" Core..!= Core.mempty)
