{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeResourceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeResourceUpdate
  ( ComputeResourceUpdate (..),

    -- * Smart constructor
    mkComputeResourceUpdate,

    -- * Lenses
    cruDesiredvCpus,
    cruMaxvCpus,
    cruMinvCpus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing the attributes of a compute environment that can be updated.
--
-- /See:/ 'mkComputeResourceUpdate' smart constructor.
data ComputeResourceUpdate = ComputeResourceUpdate'
  { -- | The desired number of Amazon EC2 vCPUS in the compute environment.
    desiredvCpus :: Core.Maybe Core.Int,
    -- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
    maxvCpus :: Core.Maybe Core.Int,
    -- | The minimum number of Amazon EC2 vCPUs that an environment should maintain.
    minvCpus :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeResourceUpdate' value with any optional fields omitted.
mkComputeResourceUpdate ::
  ComputeResourceUpdate
mkComputeResourceUpdate =
  ComputeResourceUpdate'
    { desiredvCpus = Core.Nothing,
      maxvCpus = Core.Nothing,
      minvCpus = Core.Nothing
    }

-- | The desired number of Amazon EC2 vCPUS in the compute environment.
--
-- /Note:/ Consider using 'desiredvCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cruDesiredvCpus :: Lens.Lens' ComputeResourceUpdate (Core.Maybe Core.Int)
cruDesiredvCpus = Lens.field @"desiredvCpus"
{-# DEPRECATED cruDesiredvCpus "Use generic-lens or generic-optics with 'desiredvCpus' instead." #-}

-- | The maximum number of Amazon EC2 vCPUs that an environment can reach.
--
-- /Note:/ Consider using 'maxvCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cruMaxvCpus :: Lens.Lens' ComputeResourceUpdate (Core.Maybe Core.Int)
cruMaxvCpus = Lens.field @"maxvCpus"
{-# DEPRECATED cruMaxvCpus "Use generic-lens or generic-optics with 'maxvCpus' instead." #-}

-- | The minimum number of Amazon EC2 vCPUs that an environment should maintain.
--
-- /Note:/ Consider using 'minvCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cruMinvCpus :: Lens.Lens' ComputeResourceUpdate (Core.Maybe Core.Int)
cruMinvCpus = Lens.field @"minvCpus"
{-# DEPRECATED cruMinvCpus "Use generic-lens or generic-optics with 'minvCpus' instead." #-}

instance Core.FromJSON ComputeResourceUpdate where
  toJSON ComputeResourceUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("desiredvCpus" Core..=) Core.<$> desiredvCpus,
            ("maxvCpus" Core..=) Core.<$> maxvCpus,
            ("minvCpus" Core..=) Core.<$> minvCpus
          ]
      )
