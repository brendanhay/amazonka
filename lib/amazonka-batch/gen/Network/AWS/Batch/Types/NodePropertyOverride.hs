{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodePropertyOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.NodePropertyOverride
  ( NodePropertyOverride (..)
  -- * Smart constructor
  , mkNodePropertyOverride
  -- * Lenses
  , npoTargetNodes
  , npoContainerOverrides
  ) where

import qualified Network.AWS.Batch.Types.ContainerOverrides as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Object representing any node overrides to a job definition that is used in a 'SubmitJob' API operation.
--
-- /See:/ 'mkNodePropertyOverride' smart constructor.
data NodePropertyOverride = NodePropertyOverride'
  { targetNodes :: Core.Text
    -- ^ The range of nodes, using node index values, with which to override. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range.
  , containerOverrides :: Core.Maybe Types.ContainerOverrides
    -- ^ The overrides that should be sent to a node range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodePropertyOverride' value with any optional fields omitted.
mkNodePropertyOverride
    :: Core.Text -- ^ 'targetNodes'
    -> NodePropertyOverride
mkNodePropertyOverride targetNodes
  = NodePropertyOverride'{targetNodes,
                          containerOverrides = Core.Nothing}

-- | The range of nodes, using node index values, with which to override. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range.
--
-- /Note:/ Consider using 'targetNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npoTargetNodes :: Lens.Lens' NodePropertyOverride Core.Text
npoTargetNodes = Lens.field @"targetNodes"
{-# INLINEABLE npoTargetNodes #-}
{-# DEPRECATED targetNodes "Use generic-lens or generic-optics with 'targetNodes' instead"  #-}

-- | The overrides that should be sent to a node range.
--
-- /Note:/ Consider using 'containerOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npoContainerOverrides :: Lens.Lens' NodePropertyOverride (Core.Maybe Types.ContainerOverrides)
npoContainerOverrides = Lens.field @"containerOverrides"
{-# INLINEABLE npoContainerOverrides #-}
{-# DEPRECATED containerOverrides "Use generic-lens or generic-optics with 'containerOverrides' instead"  #-}

instance Core.FromJSON NodePropertyOverride where
        toJSON NodePropertyOverride{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("targetNodes" Core..= targetNodes),
                  ("containerOverrides" Core..=) Core.<$> containerOverrides])
