{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
  ( DevicePoolCompatibilityResult (..),

    -- * Smart constructor
    mkDevicePoolCompatibilityResult,

    -- * Lenses
    dpcrCompatible,
    dpcrDevice,
    dpcrIncompatibilityMessages,
  )
where

import qualified Network.AWS.DeviceFarm.Types.Device as Types
import qualified Network.AWS.DeviceFarm.Types.IncompatibilityMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a device pool compatibility result.
--
-- /See:/ 'mkDevicePoolCompatibilityResult' smart constructor.
data DevicePoolCompatibilityResult = DevicePoolCompatibilityResult'
  { -- | Whether the result was compatible with the device pool.
    compatible :: Core.Maybe Core.Bool,
    -- | The device (phone or tablet) to return information about.
    device :: Core.Maybe Types.Device,
    -- | Information about the compatibility.
    incompatibilityMessages :: Core.Maybe [Types.IncompatibilityMessage]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DevicePoolCompatibilityResult' value with any optional fields omitted.
mkDevicePoolCompatibilityResult ::
  DevicePoolCompatibilityResult
mkDevicePoolCompatibilityResult =
  DevicePoolCompatibilityResult'
    { compatible = Core.Nothing,
      device = Core.Nothing,
      incompatibilityMessages = Core.Nothing
    }

-- | Whether the result was compatible with the device pool.
--
-- /Note:/ Consider using 'compatible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpcrCompatible :: Lens.Lens' DevicePoolCompatibilityResult (Core.Maybe Core.Bool)
dpcrCompatible = Lens.field @"compatible"
{-# DEPRECATED dpcrCompatible "Use generic-lens or generic-optics with 'compatible' instead." #-}

-- | The device (phone or tablet) to return information about.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpcrDevice :: Lens.Lens' DevicePoolCompatibilityResult (Core.Maybe Types.Device)
dpcrDevice = Lens.field @"device"
{-# DEPRECATED dpcrDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | Information about the compatibility.
--
-- /Note:/ Consider using 'incompatibilityMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpcrIncompatibilityMessages :: Lens.Lens' DevicePoolCompatibilityResult (Core.Maybe [Types.IncompatibilityMessage])
dpcrIncompatibilityMessages = Lens.field @"incompatibilityMessages"
{-# DEPRECATED dpcrIncompatibilityMessages "Use generic-lens or generic-optics with 'incompatibilityMessages' instead." #-}

instance Core.FromJSON DevicePoolCompatibilityResult where
  parseJSON =
    Core.withObject "DevicePoolCompatibilityResult" Core.$
      \x ->
        DevicePoolCompatibilityResult'
          Core.<$> (x Core..:? "compatible")
          Core.<*> (x Core..:? "device")
          Core.<*> (x Core..:? "incompatibilityMessages")
