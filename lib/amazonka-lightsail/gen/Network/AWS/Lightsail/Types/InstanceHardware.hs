{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHardware
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceHardware
  ( InstanceHardware (..)
  -- * Smart constructor
  , mkInstanceHardware
  -- * Lenses
  , ihCpuCount
  , ihDisks
  , ihRamSizeInGb
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Disk as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the hardware for the instance.
--
-- /See:/ 'mkInstanceHardware' smart constructor.
data InstanceHardware = InstanceHardware'
  { cpuCount :: Core.Maybe Core.Int
    -- ^ The number of vCPUs the instance has.
  , disks :: Core.Maybe [Types.Disk]
    -- ^ The disks attached to the instance.
  , ramSizeInGb :: Core.Maybe Core.Double
    -- ^ The amount of RAM in GB on the instance (e.g., @1.0@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceHardware' value with any optional fields omitted.
mkInstanceHardware
    :: InstanceHardware
mkInstanceHardware
  = InstanceHardware'{cpuCount = Core.Nothing, disks = Core.Nothing,
                      ramSizeInGb = Core.Nothing}

-- | The number of vCPUs the instance has.
--
-- /Note:/ Consider using 'cpuCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihCpuCount :: Lens.Lens' InstanceHardware (Core.Maybe Core.Int)
ihCpuCount = Lens.field @"cpuCount"
{-# INLINEABLE ihCpuCount #-}
{-# DEPRECATED cpuCount "Use generic-lens or generic-optics with 'cpuCount' instead"  #-}

-- | The disks attached to the instance.
--
-- /Note:/ Consider using 'disks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihDisks :: Lens.Lens' InstanceHardware (Core.Maybe [Types.Disk])
ihDisks = Lens.field @"disks"
{-# INLINEABLE ihDisks #-}
{-# DEPRECATED disks "Use generic-lens or generic-optics with 'disks' instead"  #-}

-- | The amount of RAM in GB on the instance (e.g., @1.0@ ).
--
-- /Note:/ Consider using 'ramSizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ihRamSizeInGb :: Lens.Lens' InstanceHardware (Core.Maybe Core.Double)
ihRamSizeInGb = Lens.field @"ramSizeInGb"
{-# INLINEABLE ihRamSizeInGb #-}
{-# DEPRECATED ramSizeInGb "Use generic-lens or generic-optics with 'ramSizeInGb' instead"  #-}

instance Core.FromJSON InstanceHardware where
        parseJSON
          = Core.withObject "InstanceHardware" Core.$
              \ x ->
                InstanceHardware' Core.<$>
                  (x Core..:? "cpuCount") Core.<*> x Core..:? "disks" Core.<*>
                    x Core..:? "ramSizeInGb"
