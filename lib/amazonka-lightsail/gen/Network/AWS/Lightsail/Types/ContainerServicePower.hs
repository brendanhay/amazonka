{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServicePower
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.ContainerServicePower
  ( ContainerServicePower (..)
  -- * Smart constructor
  , mkContainerServicePower
  -- * Lenses
  , cspCpuCount
  , cspIsActive
  , cspName
  , cspPowerId
  , cspPrice
  , cspRamSizeInGb
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the powers that can be specified for an Amazon Lightsail container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base price of the container service.
--
-- /See:/ 'mkContainerServicePower' smart constructor.
data ContainerServicePower = ContainerServicePower'
  { cpuCount :: Core.Maybe Core.Double
    -- ^ The number of vCPUs included in the power.
  , isActive :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the power is active and can be specified for container services.
  , name :: Core.Maybe Core.Text
    -- ^ The friendly name of the power (e.g., @nano@ ).
  , powerId :: Core.Maybe Core.Text
    -- ^ The ID of the power (e.g., @nano-1@ ).
  , price :: Core.Maybe Core.Double
    -- ^ The monthly price of the power in USD.
  , ramSizeInGb :: Core.Maybe Core.Double
    -- ^ The amount of RAM (in GB) of the power.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerServicePower' value with any optional fields omitted.
mkContainerServicePower
    :: ContainerServicePower
mkContainerServicePower
  = ContainerServicePower'{cpuCount = Core.Nothing,
                           isActive = Core.Nothing, name = Core.Nothing,
                           powerId = Core.Nothing, price = Core.Nothing,
                           ramSizeInGb = Core.Nothing}

-- | The number of vCPUs included in the power.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspCpuCount :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Double)
cspCpuCount = Lens.field @"cpuCount"
{-# INLINEABLE cspCpuCount #-}
{-# DEPRECATED cpuCount "Use generic-lens or generic-optics with 'cpuCount' instead"  #-}

-- | A Boolean value indicating whether the power is active and can be specified for container services.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspIsActive :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Bool)
cspIsActive = Lens.field @"isActive"
{-# INLINEABLE cspIsActive #-}
{-# DEPRECATED isActive "Use generic-lens or generic-optics with 'isActive' instead"  #-}

-- | The friendly name of the power (e.g., @nano@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspName :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Text)
cspName = Lens.field @"name"
{-# INLINEABLE cspName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the power (e.g., @nano-1@ ).
--
-- /Note:/ Consider using 'powerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspPowerId :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Text)
cspPowerId = Lens.field @"powerId"
{-# INLINEABLE cspPowerId #-}
{-# DEPRECATED powerId "Use generic-lens or generic-optics with 'powerId' instead"  #-}

-- | The monthly price of the power in USD.
--
-- /Note:/ Consider using 'price' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspPrice :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Double)
cspPrice = Lens.field @"price"
{-# INLINEABLE cspPrice #-}
{-# DEPRECATED price "Use generic-lens or generic-optics with 'price' instead"  #-}

-- | The amount of RAM (in GB) of the power.
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cspRamSizeInGb :: Lens.Lens' ContainerServicePower (Core.Maybe Core.Double)
cspRamSizeInGb = Lens.field @"ramSizeInGb"
{-# INLINEABLE cspRamSizeInGb #-}
{-# DEPRECATED ramSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead"  #-}

instance Core.FromJSON ContainerServicePower where
        parseJSON
          = Core.withObject "ContainerServicePower" Core.$
              \ x ->
                ContainerServicePower' Core.<$>
                  (x Core..:? "cpuCount") Core.<*> x Core..:? "isActive" Core.<*>
                    x Core..:? "name"
                    Core.<*> x Core..:? "powerId"
                    Core.<*> x Core..:? "price"
                    Core.<*> x Core..:? "ramSizeInGb"
