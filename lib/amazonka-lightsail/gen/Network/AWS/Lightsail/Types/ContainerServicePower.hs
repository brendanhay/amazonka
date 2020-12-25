{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServicePower
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServicePower
  ( ContainerServicePower (..),

    -- * Smart constructor
    mkContainerServicePower,

    -- * Lenses
    cspCpuCount,
    cspIsActive,
    cspName,
    cspPowerId,
    cspPrice,
    cspRamSizeInGb,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the powers that can be specified for an Amazon Lightsail container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base price of the container service.
--
-- /See:/ 'mkContainerServicePower' smart constructor.
data ContainerServicePower = ContainerServicePower'
  { -- | The number of vCPUs included in the power.
    cpuCount :: Core.Maybe Core.Double,
    -- | A Boolean value indicating whether the power is active and can be specified for container services.
    isActive :: Core.Maybe Core.Bool,
    -- | The friendly name of the power (e.g., @nano@ ).
    name :: Core.Maybe Types.String,
    -- | The ID of the power (e.g., @nano-1@ ).
    powerId :: Core.Maybe Types.String,
    -- | The monthly price of the power in USD.
    price :: Core.Maybe Core.Double,
    -- | The amount of RAM (in GB) of the power.
    ramSizeInGb :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerServicePower' value with any optional fields omitted.
mkContainerServicePower ::
  ContainerServicePower
mkContainerServicePower =
  ContainerServicePower'
    { cpuCount = Core.Nothing,
      isActive = Core.Nothing,
      name = Core.Nothing,
      powerId = Core.Nothing,
      price = Core.Nothing,
      ramSizeInGb = Core.Nothing
    }

-- | The number of vCPUs included in the power.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspCpuCount :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Double)
cspCpuCount = Lens.field @"cpuCount"
{-# DEPRECATED cspCpuCount "Use generic-lens or generic-optics with 'cpuCount' instead." #-}

-- | A Boolean value indicating whether the power is active and can be specified for container services.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspIsActive :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Bool)
cspIsActive = Lens.field @"isActive"
{-# DEPRECATED cspIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | The friendly name of the power (e.g., @nano@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspName :: Lens.Lens' ContainerServicePower (Core.Maybe Types.String)
cspName = Lens.field @"name"
{-# DEPRECATED cspName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the power (e.g., @nano-1@ ).
--
-- /Note:/ Consider using 'powerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspPowerId :: Lens.Lens' ContainerServicePower (Core.Maybe Types.String)
cspPowerId = Lens.field @"powerId"
{-# DEPRECATED cspPowerId "Use generic-lens or generic-optics with 'powerId' instead." #-}

-- | The monthly price of the power in USD.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspPrice :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Double)
cspPrice = Lens.field @"price"
{-# DEPRECATED cspPrice "Use generic-lens or generic-optics with 'price' instead." #-}

-- | The amount of RAM (in GB) of the power.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspRamSizeInGb :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Double)
cspRamSizeInGb = Lens.field @"ramSizeInGb"
{-# DEPRECATED cspRamSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead." #-}

instance Core.FromJSON ContainerServicePower where
  parseJSON =
    Core.withObject "ContainerServicePower" Core.$
      \x ->
        ContainerServicePower'
          Core.<$> (x Core..:? "cpuCount")
          Core.<*> (x Core..:? "isActive")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "powerId")
          Core.<*> (x Core..:? "price")
          Core.<*> (x Core..:? "ramSizeInGb")
