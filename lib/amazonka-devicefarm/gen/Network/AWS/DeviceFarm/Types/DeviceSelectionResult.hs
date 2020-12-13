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
    dsrMatchedDevicesCount,
    dsrFilters,
    dsrMaxDevices,
  )
where

import Network.AWS.DeviceFarm.Types.DeviceFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the run results requested by the device selection configuration and how many devices were returned. For an example of the JSON response syntax, see 'ScheduleRun' .
--
-- /See:/ 'mkDeviceSelectionResult' smart constructor.
data DeviceSelectionResult = DeviceSelectionResult'
  { -- | The number of devices that matched the device filter selection criteria.
    matchedDevicesCount :: Lude.Maybe Lude.Int,
    -- | The filters in a device selection result.
    filters :: Lude.Maybe [DeviceFilter],
    -- | The maximum number of devices to be selected by a device filter and included in a test run.
    maxDevices :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceSelectionResult' with the minimum fields required to make a request.
--
-- * 'matchedDevicesCount' - The number of devices that matched the device filter selection criteria.
-- * 'filters' - The filters in a device selection result.
-- * 'maxDevices' - The maximum number of devices to be selected by a device filter and included in a test run.
mkDeviceSelectionResult ::
  DeviceSelectionResult
mkDeviceSelectionResult =
  DeviceSelectionResult'
    { matchedDevicesCount = Lude.Nothing,
      filters = Lude.Nothing,
      maxDevices = Lude.Nothing
    }

-- | The number of devices that matched the device filter selection criteria.
--
-- /Note:/ Consider using 'matchedDevicesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMatchedDevicesCount :: Lens.Lens' DeviceSelectionResult (Lude.Maybe Lude.Int)
dsrMatchedDevicesCount = Lens.lens (matchedDevicesCount :: DeviceSelectionResult -> Lude.Maybe Lude.Int) (\s a -> s {matchedDevicesCount = a} :: DeviceSelectionResult)
{-# DEPRECATED dsrMatchedDevicesCount "Use generic-lens or generic-optics with 'matchedDevicesCount' instead." #-}

-- | The filters in a device selection result.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrFilters :: Lens.Lens' DeviceSelectionResult (Lude.Maybe [DeviceFilter])
dsrFilters = Lens.lens (filters :: DeviceSelectionResult -> Lude.Maybe [DeviceFilter]) (\s a -> s {filters = a} :: DeviceSelectionResult)
{-# DEPRECATED dsrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of devices to be selected by a device filter and included in a test run.
--
-- /Note:/ Consider using 'maxDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrMaxDevices :: Lens.Lens' DeviceSelectionResult (Lude.Maybe Lude.Int)
dsrMaxDevices = Lens.lens (maxDevices :: DeviceSelectionResult -> Lude.Maybe Lude.Int) (\s a -> s {maxDevices = a} :: DeviceSelectionResult)
{-# DEPRECATED dsrMaxDevices "Use generic-lens or generic-optics with 'maxDevices' instead." #-}

instance Lude.FromJSON DeviceSelectionResult where
  parseJSON =
    Lude.withObject
      "DeviceSelectionResult"
      ( \x ->
          DeviceSelectionResult'
            Lude.<$> (x Lude..:? "matchedDevicesCount")
            Lude.<*> (x Lude..:? "filters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "maxDevices")
      )
