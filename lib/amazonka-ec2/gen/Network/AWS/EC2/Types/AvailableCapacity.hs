{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AvailableCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AvailableCapacity
  ( AvailableCapacity (..),

    -- * Smart constructor
    mkAvailableCapacity,

    -- * Lenses
    acAvailableInstanceCapacity,
    acAvailableVCpus,
  )
where

import qualified Network.AWS.EC2.Types.InstanceCapacity as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The capacity information for instances that can be launched onto the Dedicated Host.
--
-- /See:/ 'mkAvailableCapacity' smart constructor.
data AvailableCapacity = AvailableCapacity'
  { -- | The number of instances that can be launched onto the Dedicated Host depending on the host's available capacity. For Dedicated Hosts that support multiple instance types, this parameter represents the number of instances for each instance size that is supported on the host.
    availableInstanceCapacity :: Core.Maybe [Types.InstanceCapacity],
    -- | The number of vCPUs available for launching instances onto the Dedicated Host.
    availableVCpus :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailableCapacity' value with any optional fields omitted.
mkAvailableCapacity ::
  AvailableCapacity
mkAvailableCapacity =
  AvailableCapacity'
    { availableInstanceCapacity = Core.Nothing,
      availableVCpus = Core.Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host depending on the host's available capacity. For Dedicated Hosts that support multiple instance types, this parameter represents the number of instances for each instance size that is supported on the host.
--
-- /Note:/ Consider using 'availableInstanceCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAvailableInstanceCapacity :: Lens.Lens' AvailableCapacity (Core.Maybe [Types.InstanceCapacity])
acAvailableInstanceCapacity = Lens.field @"availableInstanceCapacity"
{-# DEPRECATED acAvailableInstanceCapacity "Use generic-lens or generic-optics with 'availableInstanceCapacity' instead." #-}

-- | The number of vCPUs available for launching instances onto the Dedicated Host.
--
-- /Note:/ Consider using 'availableVCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAvailableVCpus :: Lens.Lens' AvailableCapacity (Core.Maybe Core.Int)
acAvailableVCpus = Lens.field @"availableVCpus"
{-# DEPRECATED acAvailableVCpus "Use generic-lens or generic-optics with 'availableVCpus' instead." #-}

instance Core.FromXML AvailableCapacity where
  parseXML x =
    AvailableCapacity'
      Core.<$> ( x Core..@? "availableInstanceCapacity"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "availableVCpus")
