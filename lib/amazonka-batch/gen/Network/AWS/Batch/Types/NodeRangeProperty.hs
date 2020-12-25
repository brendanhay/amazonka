{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeRangeProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeRangeProperty
  ( NodeRangeProperty (..),

    -- * Smart constructor
    mkNodeRangeProperty,

    -- * Lenses
    nrpTargetNodes,
    nrpContainer,
  )
where

import qualified Network.AWS.Batch.Types.ContainerProperties as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the properties of the node range for a multi-node parallel job.
--
-- /See:/ 'mkNodeRangeProperty' smart constructor.
data NodeRangeProperty = NodeRangeProperty'
  { -- | The range of nodes, using node index values. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range. Your accumulative node ranges must account for all nodes (0:n). You may nest node ranges, for example 0:10 and 4:5, in which case the 4:5 range properties override the 0:10 properties.
    targetNodes :: Types.String,
    -- | The container details for the node range.
    container :: Core.Maybe Types.ContainerProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeRangeProperty' value with any optional fields omitted.
mkNodeRangeProperty ::
  -- | 'targetNodes'
  Types.String ->
  NodeRangeProperty
mkNodeRangeProperty targetNodes =
  NodeRangeProperty' {targetNodes, container = Core.Nothing}

-- | The range of nodes, using node index values. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range. Your accumulative node ranges must account for all nodes (0:n). You may nest node ranges, for example 0:10 and 4:5, in which case the 4:5 range properties override the 0:10 properties.
--
-- /Note:/ Consider using 'targetNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrpTargetNodes :: Lens.Lens' NodeRangeProperty Types.String
nrpTargetNodes = Lens.field @"targetNodes"
{-# DEPRECATED nrpTargetNodes "Use generic-lens or generic-optics with 'targetNodes' instead." #-}

-- | The container details for the node range.
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nrpContainer :: Lens.Lens' NodeRangeProperty (Core.Maybe Types.ContainerProperties)
nrpContainer = Lens.field @"container"
{-# DEPRECATED nrpContainer "Use generic-lens or generic-optics with 'container' instead." #-}

instance Core.FromJSON NodeRangeProperty where
  toJSON NodeRangeProperty {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("targetNodes" Core..= targetNodes),
            ("container" Core..=) Core.<$> container
          ]
      )

instance Core.FromJSON NodeRangeProperty where
  parseJSON =
    Core.withObject "NodeRangeProperty" Core.$
      \x ->
        NodeRangeProperty'
          Core.<$> (x Core..: "targetNodes") Core.<*> (x Core..:? "container")
