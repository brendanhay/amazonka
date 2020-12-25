{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceNetworking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceNetworking
  ( InstanceNetworking (..),

    -- * Smart constructor
    mkInstanceNetworking,

    -- * Lenses
    inMonthlyTransfer,
    inPorts,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.InstancePortInfo as Types
import qualified Network.AWS.Lightsail.Types.MonthlyTransfer as Types
import qualified Network.AWS.Prelude as Core

-- | Describes monthly data transfer rates and port information for an instance.
--
-- /See:/ 'mkInstanceNetworking' smart constructor.
data InstanceNetworking = InstanceNetworking'
  { -- | The amount of data in GB allocated for monthly data transfers.
    monthlyTransfer :: Core.Maybe Types.MonthlyTransfer,
    -- | An array of key-value pairs containing information about the ports on the instance.
    ports :: Core.Maybe [Types.InstancePortInfo]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceNetworking' value with any optional fields omitted.
mkInstanceNetworking ::
  InstanceNetworking
mkInstanceNetworking =
  InstanceNetworking'
    { monthlyTransfer = Core.Nothing,
      ports = Core.Nothing
    }

-- | The amount of data in GB allocated for monthly data transfers.
--
-- /Note:/ Consider using 'monthlyTransfer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inMonthlyTransfer :: Lens.Lens' InstanceNetworking (Core.Maybe Types.MonthlyTransfer)
inMonthlyTransfer = Lens.field @"monthlyTransfer"
{-# DEPRECATED inMonthlyTransfer "Use generic-lens or generic-optics with 'monthlyTransfer' instead." #-}

-- | An array of key-value pairs containing information about the ports on the instance.
--
-- /Note:/ Consider using 'ports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inPorts :: Lens.Lens' InstanceNetworking (Core.Maybe [Types.InstancePortInfo])
inPorts = Lens.field @"ports"
{-# DEPRECATED inPorts "Use generic-lens or generic-optics with 'ports' instead." #-}

instance Core.FromJSON InstanceNetworking where
  parseJSON =
    Core.withObject "InstanceNetworking" Core.$
      \x ->
        InstanceNetworking'
          Core.<$> (x Core..:? "monthlyTransfer") Core.<*> (x Core..:? "ports")
