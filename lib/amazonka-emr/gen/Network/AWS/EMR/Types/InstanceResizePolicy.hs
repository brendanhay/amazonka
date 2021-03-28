{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceResizePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceResizePolicy
  ( InstanceResizePolicy (..)
  -- * Smart constructor
  , mkInstanceResizePolicy
  -- * Lenses
  , irpInstanceTerminationTimeout
  , irpInstancesToProtect
  , irpInstancesToTerminate
  ) where

import qualified Network.AWS.EMR.Types.InstanceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Custom policy for requesting termination protection or termination of specific instances when shrinking an instance group.
--
-- /See:/ 'mkInstanceResizePolicy' smart constructor.
data InstanceResizePolicy = InstanceResizePolicy'
  { instanceTerminationTimeout :: Core.Maybe Core.Int
    -- ^ Decommissioning timeout override for the specific list of instances to be terminated.
  , instancesToProtect :: Core.Maybe [Types.InstanceId]
    -- ^ Specific list of instances to be protected when shrinking an instance group.
  , instancesToTerminate :: Core.Maybe [Types.InstanceId]
    -- ^ Specific list of instances to be terminated when shrinking an instance group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceResizePolicy' value with any optional fields omitted.
mkInstanceResizePolicy
    :: InstanceResizePolicy
mkInstanceResizePolicy
  = InstanceResizePolicy'{instanceTerminationTimeout = Core.Nothing,
                          instancesToProtect = Core.Nothing,
                          instancesToTerminate = Core.Nothing}

-- | Decommissioning timeout override for the specific list of instances to be terminated.
--
-- /Note:/ Consider using 'instanceTerminationTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irpInstanceTerminationTimeout :: Lens.Lens' InstanceResizePolicy (Core.Maybe Core.Int)
irpInstanceTerminationTimeout = Lens.field @"instanceTerminationTimeout"
{-# INLINEABLE irpInstanceTerminationTimeout #-}
{-# DEPRECATED instanceTerminationTimeout "Use generic-lens or generic-optics with 'instanceTerminationTimeout' instead"  #-}

-- | Specific list of instances to be protected when shrinking an instance group.
--
-- /Note:/ Consider using 'instancesToProtect' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irpInstancesToProtect :: Lens.Lens' InstanceResizePolicy (Core.Maybe [Types.InstanceId])
irpInstancesToProtect = Lens.field @"instancesToProtect"
{-# INLINEABLE irpInstancesToProtect #-}
{-# DEPRECATED instancesToProtect "Use generic-lens or generic-optics with 'instancesToProtect' instead"  #-}

-- | Specific list of instances to be terminated when shrinking an instance group.
--
-- /Note:/ Consider using 'instancesToTerminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
irpInstancesToTerminate :: Lens.Lens' InstanceResizePolicy (Core.Maybe [Types.InstanceId])
irpInstancesToTerminate = Lens.field @"instancesToTerminate"
{-# INLINEABLE irpInstancesToTerminate #-}
{-# DEPRECATED instancesToTerminate "Use generic-lens or generic-optics with 'instancesToTerminate' instead"  #-}

instance Core.FromJSON InstanceResizePolicy where
        toJSON InstanceResizePolicy{..}
          = Core.object
              (Core.catMaybes
                 [("InstanceTerminationTimeout" Core..=) Core.<$>
                    instanceTerminationTimeout,
                  ("InstancesToProtect" Core..=) Core.<$> instancesToProtect,
                  ("InstancesToTerminate" Core..=) Core.<$> instancesToTerminate])

instance Core.FromJSON InstanceResizePolicy where
        parseJSON
          = Core.withObject "InstanceResizePolicy" Core.$
              \ x ->
                InstanceResizePolicy' Core.<$>
                  (x Core..:? "InstanceTerminationTimeout") Core.<*>
                    x Core..:? "InstancesToProtect"
                    Core.<*> x Core..:? "InstancesToTerminate"
