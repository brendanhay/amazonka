{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCapacity
  ( InstanceCapacity (..),

    -- * Smart constructor
    mkInstanceCapacity,

    -- * Lenses
    icAvailableCapacity,
    icInstanceType,
    icTotalCapacity,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the number of instances that can be launched onto the Dedicated Host.
--
-- /See:/ 'mkInstanceCapacity' smart constructor.
data InstanceCapacity = InstanceCapacity'
  { -- | The number of instances that can be launched onto the Dedicated Host based on the host's available capacity.
    availableCapacity :: Core.Maybe Core.Int,
    -- | The instance type supported by the Dedicated Host.
    instanceType :: Core.Maybe Types.String,
    -- | The total number of instances that can be launched onto the Dedicated Host if there are no instances running on it.
    totalCapacity :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceCapacity' value with any optional fields omitted.
mkInstanceCapacity ::
  InstanceCapacity
mkInstanceCapacity =
  InstanceCapacity'
    { availableCapacity = Core.Nothing,
      instanceType = Core.Nothing,
      totalCapacity = Core.Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host based on the host's available capacity.
--
-- /Note:/ Consider using 'availableCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icAvailableCapacity :: Lens.Lens' InstanceCapacity (Core.Maybe Core.Int)
icAvailableCapacity = Lens.field @"availableCapacity"
{-# DEPRECATED icAvailableCapacity "Use generic-lens or generic-optics with 'availableCapacity' instead." #-}

-- | The instance type supported by the Dedicated Host.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icInstanceType :: Lens.Lens' InstanceCapacity (Core.Maybe Types.String)
icInstanceType = Lens.field @"instanceType"
{-# DEPRECATED icInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The total number of instances that can be launched onto the Dedicated Host if there are no instances running on it.
--
-- /Note:/ Consider using 'totalCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icTotalCapacity :: Lens.Lens' InstanceCapacity (Core.Maybe Core.Int)
icTotalCapacity = Lens.field @"totalCapacity"
{-# DEPRECATED icTotalCapacity "Use generic-lens or generic-optics with 'totalCapacity' instead." #-}

instance Core.FromXML InstanceCapacity where
  parseXML x =
    InstanceCapacity'
      Core.<$> (x Core..@? "availableCapacity")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "totalCapacity")
