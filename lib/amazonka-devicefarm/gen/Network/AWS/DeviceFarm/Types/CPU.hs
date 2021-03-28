{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.CPU
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.CPU
  ( CPU (..)
  -- * Smart constructor
  , mkCPU
  -- * Lenses
  , cpuArchitecture
  , cpuClock
  , cpuFrequency
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the amount of CPU that an app is using on a physical device. Does not represent system-wide CPU usage.
--
-- /See:/ 'mkCPU' smart constructor.
data CPU = CPU'
  { architecture :: Core.Maybe Core.Text
    -- ^ The CPU's architecture (for example, x86 or ARM).
  , clock :: Core.Maybe Core.Double
    -- ^ The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
  , frequency :: Core.Maybe Core.Text
    -- ^ The CPU's frequency.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CPU' value with any optional fields omitted.
mkCPU
    :: CPU
mkCPU
  = CPU'{architecture = Core.Nothing, clock = Core.Nothing,
         frequency = Core.Nothing}

-- | The CPU's architecture (for example, x86 or ARM).
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuArchitecture :: Lens.Lens' CPU (Core.Maybe Core.Text)
cpuArchitecture = Lens.field @"architecture"
{-# INLINEABLE cpuArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | The clock speed of the device's CPU, expressed in hertz (Hz). For example, a 1.2 GHz CPU is expressed as 1200000000.
--
-- /Note:/ Consider using 'clock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuClock :: Lens.Lens' CPU (Core.Maybe Core.Double)
cpuClock = Lens.field @"clock"
{-# INLINEABLE cpuClock #-}
{-# DEPRECATED clock "Use generic-lens or generic-optics with 'clock' instead"  #-}

-- | The CPU's frequency.
--
-- /Note:/ Consider using 'frequency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpuFrequency :: Lens.Lens' CPU (Core.Maybe Core.Text)
cpuFrequency = Lens.field @"frequency"
{-# INLINEABLE cpuFrequency #-}
{-# DEPRECATED frequency "Use generic-lens or generic-optics with 'frequency' instead"  #-}

instance Core.FromJSON CPU where
        parseJSON
          = Core.withObject "CPU" Core.$
              \ x ->
                CPU' Core.<$>
                  (x Core..:? "architecture") Core.<*> x Core..:? "clock" Core.<*>
                    x Core..:? "frequency"
