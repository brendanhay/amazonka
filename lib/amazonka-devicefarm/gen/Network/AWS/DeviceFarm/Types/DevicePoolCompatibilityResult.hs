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
    dpcrDevice,
    dpcrCompatible,
    dpcrIncompatibilityMessages,
  )
where

import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a device pool compatibility result.
--
-- /See:/ 'mkDevicePoolCompatibilityResult' smart constructor.
data DevicePoolCompatibilityResult = DevicePoolCompatibilityResult'
  { device ::
      Lude.Maybe Device,
    compatible ::
      Lude.Maybe Lude.Bool,
    incompatibilityMessages ::
      Lude.Maybe
        [IncompatibilityMessage]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DevicePoolCompatibilityResult' with the minimum fields required to make a request.
--
-- * 'compatible' - Whether the result was compatible with the device pool.
-- * 'device' - The device (phone or tablet) to return information about.
-- * 'incompatibilityMessages' - Information about the compatibility.
mkDevicePoolCompatibilityResult ::
  DevicePoolCompatibilityResult
mkDevicePoolCompatibilityResult =
  DevicePoolCompatibilityResult'
    { device = Lude.Nothing,
      compatible = Lude.Nothing,
      incompatibilityMessages = Lude.Nothing
    }

-- | The device (phone or tablet) to return information about.
--
-- /Note:/ Consider using 'device' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpcrDevice :: Lens.Lens' DevicePoolCompatibilityResult (Lude.Maybe Device)
dpcrDevice = Lens.lens (device :: DevicePoolCompatibilityResult -> Lude.Maybe Device) (\s a -> s {device = a} :: DevicePoolCompatibilityResult)
{-# DEPRECATED dpcrDevice "Use generic-lens or generic-optics with 'device' instead." #-}

-- | Whether the result was compatible with the device pool.
--
-- /Note:/ Consider using 'compatible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpcrCompatible :: Lens.Lens' DevicePoolCompatibilityResult (Lude.Maybe Lude.Bool)
dpcrCompatible = Lens.lens (compatible :: DevicePoolCompatibilityResult -> Lude.Maybe Lude.Bool) (\s a -> s {compatible = a} :: DevicePoolCompatibilityResult)
{-# DEPRECATED dpcrCompatible "Use generic-lens or generic-optics with 'compatible' instead." #-}

-- | Information about the compatibility.
--
-- /Note:/ Consider using 'incompatibilityMessages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpcrIncompatibilityMessages :: Lens.Lens' DevicePoolCompatibilityResult (Lude.Maybe [IncompatibilityMessage])
dpcrIncompatibilityMessages = Lens.lens (incompatibilityMessages :: DevicePoolCompatibilityResult -> Lude.Maybe [IncompatibilityMessage]) (\s a -> s {incompatibilityMessages = a} :: DevicePoolCompatibilityResult)
{-# DEPRECATED dpcrIncompatibilityMessages "Use generic-lens or generic-optics with 'incompatibilityMessages' instead." #-}

instance Lude.FromJSON DevicePoolCompatibilityResult where
  parseJSON =
    Lude.withObject
      "DevicePoolCompatibilityResult"
      ( \x ->
          DevicePoolCompatibilityResult'
            Lude.<$> (x Lude..:? "device")
            Lude.<*> (x Lude..:? "compatible")
            Lude.<*> (x Lude..:? "incompatibilityMessages" Lude..!= Lude.mempty)
      )
