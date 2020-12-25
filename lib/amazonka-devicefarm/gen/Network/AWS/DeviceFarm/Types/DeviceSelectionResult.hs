{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceSelectionResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceSelectionResult
  ( DeviceSelectionResult (..),

    -- * Smart constructor
    mkDeviceSelectionResult,

    -- * Lenses
    dsrFilters,
    dsrMatchedDevicesCount,
    dsrMaxDevices,
  )
where

import qualified Network.AWS.DeviceFarm.Types.DeviceFilter as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the run results requested by the device selection configuration and how many devices were returned. For an example of the JSON response syntax, see 'ScheduleRun' .
--
-- /See:/ 'mkDeviceSelectionResult' smart constructor.
data DeviceSelectionResult = DeviceSelectionResult'
  { -- | The filters in a device selection result.
    filters :: Core.Maybe [Types.DeviceFilter],
    -- | The number of devices that matched the device filter selection criteria.
    matchedDevicesCount :: Core.Maybe Core.Int,
    -- | The maximum number of devices to be selected by a device filter and included in a test run.
    maxDevices :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceSelectionResult' value with any optional fields omitted.
mkDeviceSelectionResult ::
  DeviceSelectionResult
mkDeviceSelectionResult =
  DeviceSelectionResult'
    { filters = Core.Nothing,
      matchedDevicesCount = Core.Nothing,
      maxDevices = Core.Nothing
    }

-- | The filters in a device selection result.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrFilters :: Lens.Lens' DeviceSelectionResult (Core.Maybe [Types.DeviceFilter])
dsrFilters = Lens.field @"filters"
{-# DEPRECATED dsrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The number of devices that matched the device filter selection criteria.
--
-- /Note:/ Consider using 'matchedDevicesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMatchedDevicesCount :: Lens.Lens' DeviceSelectionResult (Core.Maybe Core.Int)
dsrMatchedDevicesCount = Lens.field @"matchedDevicesCount"
{-# DEPRECATED dsrMatchedDevicesCount "Use generic-lens or generic-optics with 'matchedDevicesCount' instead." #-}

-- | The maximum number of devices to be selected by a device filter and included in a test run.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMaxDevices :: Lens.Lens' DeviceSelectionResult (Core.Maybe Core.Int)
dsrMaxDevices = Lens.field @"maxDevices"
{-# DEPRECATED dsrMaxDevices "Use generic-lens or generic-optics with 'maxDevices' instead." #-}

instance Core.FromJSON DeviceSelectionResult where
  parseJSON =
    Core.withObject "DeviceSelectionResult" Core.$
      \x ->
        DeviceSelectionResult'
          Core.<$> (x Core..:? "filters")
          Core.<*> (x Core..:? "matchedDevicesCount")
          Core.<*> (x Core..:? "maxDevices")
