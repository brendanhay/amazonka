{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeEnvironmentOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Batch.Types.ComputeEnvironmentOrder
  ( ComputeEnvironmentOrder (..)
  -- * Smart constructor
  , mkComputeEnvironmentOrder
  -- * Lenses
  , ceoOrder
  , ceoComputeEnvironment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The order in which compute environments are tried for job placement within a queue. Compute environments are tried in ascending order. For example, if two compute environments are associated with a job queue, the compute environment with a lower order integer value is tried for job placement first.
--
-- /See:/ 'mkComputeEnvironmentOrder' smart constructor.
data ComputeEnvironmentOrder = ComputeEnvironmentOrder'
  { order :: Core.Int
    -- ^ The order of the compute environment.
  , computeEnvironment :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the compute environment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeEnvironmentOrder' value with any optional fields omitted.
mkComputeEnvironmentOrder
    :: Core.Int -- ^ 'order'
    -> Core.Text -- ^ 'computeEnvironment'
    -> ComputeEnvironmentOrder
mkComputeEnvironmentOrder order computeEnvironment
  = ComputeEnvironmentOrder'{order, computeEnvironment}

-- | The order of the compute environment.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoOrder :: Lens.Lens' ComputeEnvironmentOrder Core.Int
ceoOrder = Lens.field @"order"
{-# INLINEABLE ceoOrder #-}
{-# DEPRECATED order "Use generic-lens or generic-optics with 'order' instead"  #-}

-- | The Amazon Resource Name (ARN) of the compute environment.
--
-- /Note:/ Consider using 'computeEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceoComputeEnvironment :: Lens.Lens' ComputeEnvironmentOrder Core.Text
ceoComputeEnvironment = Lens.field @"computeEnvironment"
{-# INLINEABLE ceoComputeEnvironment #-}
{-# DEPRECATED computeEnvironment "Use generic-lens or generic-optics with 'computeEnvironment' instead"  #-}

instance Core.FromJSON ComputeEnvironmentOrder where
        toJSON ComputeEnvironmentOrder{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("order" Core..= order),
                  Core.Just ("computeEnvironment" Core..= computeEnvironment)])

instance Core.FromJSON ComputeEnvironmentOrder where
        parseJSON
          = Core.withObject "ComputeEnvironmentOrder" Core.$
              \ x ->
                ComputeEnvironmentOrder' Core.<$>
                  (x Core..: "order") Core.<*> x Core..: "computeEnvironment"
