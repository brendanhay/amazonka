{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.NodeOverrides
  ( NodeOverrides (..)
  -- * Smart constructor
  , mkNodeOverrides
  -- * Lenses
  , noNodePropertyOverrides
  , noNumNodes
  ) where

import qualified Network.AWS.Batch.Types.NodePropertyOverride as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Object representing any node overrides to a job definition that is used in a 'SubmitJob' API operation.
--
-- /See:/ 'mkNodeOverrides' smart constructor.
data NodeOverrides = NodeOverrides'
  { nodePropertyOverrides :: Core.Maybe [Types.NodePropertyOverride]
    -- ^ The node property overrides for the job.
  , numNodes :: Core.Maybe Core.Int
    -- ^ The number of nodes to use with a multi-node parallel job. This value overrides the number of nodes that are specified in the job definition. To use this override:
--
--
--     * There must be at least one node range in your job definition that has an open upper boundary (such as @:@ or @n:@ ).
--
--
--     * The lower boundary of the node range specified in the job definition must be fewer than the number of nodes specified in the override.
--
--
--     * The main node index specified in the job definition must be fewer than the number of nodes specified in the override.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NodeOverrides' value with any optional fields omitted.
mkNodeOverrides
    :: NodeOverrides
mkNodeOverrides
  = NodeOverrides'{nodePropertyOverrides = Core.Nothing,
                   numNodes = Core.Nothing}

-- | The node property overrides for the job.
--
-- /Note:/ Consider using 'nodePropertyOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noNodePropertyOverrides :: Lens.Lens' NodeOverrides (Core.Maybe [Types.NodePropertyOverride])
noNodePropertyOverrides = Lens.field @"nodePropertyOverrides"
{-# INLINEABLE noNodePropertyOverrides #-}
{-# DEPRECATED nodePropertyOverrides "Use generic-lens or generic-optics with 'nodePropertyOverrides' instead"  #-}

-- | The number of nodes to use with a multi-node parallel job. This value overrides the number of nodes that are specified in the job definition. To use this override:
--
--
--     * There must be at least one node range in your job definition that has an open upper boundary (such as @:@ or @n:@ ).
--
--
--     * The lower boundary of the node range specified in the job definition must be fewer than the number of nodes specified in the override.
--
--
--     * The main node index specified in the job definition must be fewer than the number of nodes specified in the override.
--
--
--
-- /Note:/ Consider using 'numNodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
noNumNodes :: Lens.Lens' NodeOverrides (Core.Maybe Core.Int)
noNumNodes = Lens.field @"numNodes"
{-# INLINEABLE noNumNodes #-}
{-# DEPRECATED numNodes "Use generic-lens or generic-optics with 'numNodes' instead"  #-}

instance Core.FromJSON NodeOverrides where
        toJSON NodeOverrides{..}
          = Core.object
              (Core.catMaybes
                 [("nodePropertyOverrides" Core..=) Core.<$> nodePropertyOverrides,
                  ("numNodes" Core..=) Core.<$> numNodes])
