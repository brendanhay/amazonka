{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Radios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Radios
  ( Radios (..)
  -- * Smart constructor
  , mkRadios
  -- * Lenses
  , rBluetooth
  , rGps
  , rNfc
  , rWifi
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.
--
-- /See:/ 'mkRadios' smart constructor.
data Radios = Radios'
  { bluetooth :: Core.Maybe Core.Bool
    -- ^ True if Bluetooth is enabled at the beginning of the test. Otherwise, false.
  , gps :: Core.Maybe Core.Bool
    -- ^ True if GPS is enabled at the beginning of the test. Otherwise, false.
  , nfc :: Core.Maybe Core.Bool
    -- ^ True if NFC is enabled at the beginning of the test. Otherwise, false.
  , wifi :: Core.Maybe Core.Bool
    -- ^ True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Radios' value with any optional fields omitted.
mkRadios
    :: Radios
mkRadios
  = Radios'{bluetooth = Core.Nothing, gps = Core.Nothing,
            nfc = Core.Nothing, wifi = Core.Nothing}

-- | True if Bluetooth is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'bluetooth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBluetooth :: Lens.Lens' Radios (Core.Maybe Core.Bool)
rBluetooth = Lens.field @"bluetooth"
{-# INLINEABLE rBluetooth #-}
{-# DEPRECATED bluetooth "Use generic-lens or generic-optics with 'bluetooth' instead"  #-}

-- | True if GPS is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'gps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGps :: Lens.Lens' Radios (Core.Maybe Core.Bool)
rGps = Lens.field @"gps"
{-# INLINEABLE rGps #-}
{-# DEPRECATED gps "Use generic-lens or generic-optics with 'gps' instead"  #-}

-- | True if NFC is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'nfc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNfc :: Lens.Lens' Radios (Core.Maybe Core.Bool)
rNfc = Lens.field @"nfc"
{-# INLINEABLE rNfc #-}
{-# DEPRECATED nfc "Use generic-lens or generic-optics with 'nfc' instead"  #-}

-- | True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'wifi' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWifi :: Lens.Lens' Radios (Core.Maybe Core.Bool)
rWifi = Lens.field @"wifi"
{-# INLINEABLE rWifi #-}
{-# DEPRECATED wifi "Use generic-lens or generic-optics with 'wifi' instead"  #-}

instance Core.FromJSON Radios where
        toJSON Radios{..}
          = Core.object
              (Core.catMaybes
                 [("bluetooth" Core..=) Core.<$> bluetooth,
                  ("gps" Core..=) Core.<$> gps, ("nfc" Core..=) Core.<$> nfc,
                  ("wifi" Core..=) Core.<$> wifi])

instance Core.FromJSON Radios where
        parseJSON
          = Core.withObject "Radios" Core.$
              \ x ->
                Radios' Core.<$>
                  (x Core..:? "bluetooth") Core.<*> x Core..:? "gps" Core.<*>
                    x Core..:? "nfc"
                    Core.<*> x Core..:? "wifi"
