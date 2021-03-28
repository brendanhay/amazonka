{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceMinutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DeviceMinutes
  ( DeviceMinutes (..)
  -- * Smart constructor
  , mkDeviceMinutes
  -- * Lenses
  , dmMetered
  , dmTotal
  , dmUnmetered
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the total (metered or unmetered) minutes used by the resource to run tests. Contains the sum of minutes consumed by all children.
--
-- /See:/ 'mkDeviceMinutes' smart constructor.
data DeviceMinutes = DeviceMinutes'
  { metered :: Core.Maybe Core.Double
    -- ^ When specified, represents only the sum of metered minutes used by the resource to run tests.
  , total :: Core.Maybe Core.Double
    -- ^ When specified, represents the total minutes used by the resource to run tests.
  , unmetered :: Core.Maybe Core.Double
    -- ^ When specified, represents only the sum of unmetered minutes used by the resource to run tests.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceMinutes' value with any optional fields omitted.
mkDeviceMinutes
    :: DeviceMinutes
mkDeviceMinutes
  = DeviceMinutes'{metered = Core.Nothing, total = Core.Nothing,
                   unmetered = Core.Nothing}

-- | When specified, represents only the sum of metered minutes used by the resource to run tests.
--
-- /Note:/ Consider using 'metered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmMetered :: Lens.Lens' DeviceMinutes (Core.Maybe Core.Double)
dmMetered = Lens.field @"metered"
{-# INLINEABLE dmMetered #-}
{-# DEPRECATED metered "Use generic-lens or generic-optics with 'metered' instead"  #-}

-- | When specified, represents the total minutes used by the resource to run tests.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmTotal :: Lens.Lens' DeviceMinutes (Core.Maybe Core.Double)
dmTotal = Lens.field @"total"
{-# INLINEABLE dmTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

-- | When specified, represents only the sum of unmetered minutes used by the resource to run tests.
--
-- /Note:/ Consider using 'unmetered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmUnmetered :: Lens.Lens' DeviceMinutes (Core.Maybe Core.Double)
dmUnmetered = Lens.field @"unmetered"
{-# INLINEABLE dmUnmetered #-}
{-# DEPRECATED unmetered "Use generic-lens or generic-optics with 'unmetered' instead"  #-}

instance Core.FromJSON DeviceMinutes where
        parseJSON
          = Core.withObject "DeviceMinutes" Core.$
              \ x ->
                DeviceMinutes' Core.<$>
                  (x Core..:? "metered") Core.<*> x Core..:? "total" Core.<*>
                    x Core..:? "unmetered"
