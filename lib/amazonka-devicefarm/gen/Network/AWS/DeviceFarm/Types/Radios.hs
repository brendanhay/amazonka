{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Radios
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Radios
  ( Radios (..),

    -- * Smart constructor
    mkRadios,

    -- * Lenses
    rNfc,
    rGps,
    rBluetooth,
    rWifi,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the set of radios and their states on a device. Examples of radios include Wi-Fi, GPS, Bluetooth, and NFC.
--
-- /See:/ 'mkRadios' smart constructor.
data Radios = Radios'
  { -- | True if NFC is enabled at the beginning of the test. Otherwise, false.
    nfc :: Lude.Maybe Lude.Bool,
    -- | True if GPS is enabled at the beginning of the test. Otherwise, false.
    gps :: Lude.Maybe Lude.Bool,
    -- | True if Bluetooth is enabled at the beginning of the test. Otherwise, false.
    bluetooth :: Lude.Maybe Lude.Bool,
    -- | True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
    wifi :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Radios' with the minimum fields required to make a request.
--
-- * 'nfc' - True if NFC is enabled at the beginning of the test. Otherwise, false.
-- * 'gps' - True if GPS is enabled at the beginning of the test. Otherwise, false.
-- * 'bluetooth' - True if Bluetooth is enabled at the beginning of the test. Otherwise, false.
-- * 'wifi' - True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
mkRadios ::
  Radios
mkRadios =
  Radios'
    { nfc = Lude.Nothing,
      gps = Lude.Nothing,
      bluetooth = Lude.Nothing,
      wifi = Lude.Nothing
    }

-- | True if NFC is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'nfc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rNfc :: Lens.Lens' Radios (Lude.Maybe Lude.Bool)
rNfc = Lens.lens (nfc :: Radios -> Lude.Maybe Lude.Bool) (\s a -> s {nfc = a} :: Radios)
{-# DEPRECATED rNfc "Use generic-lens or generic-optics with 'nfc' instead." #-}

-- | True if GPS is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'gps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGps :: Lens.Lens' Radios (Lude.Maybe Lude.Bool)
rGps = Lens.lens (gps :: Radios -> Lude.Maybe Lude.Bool) (\s a -> s {gps = a} :: Radios)
{-# DEPRECATED rGps "Use generic-lens or generic-optics with 'gps' instead." #-}

-- | True if Bluetooth is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'bluetooth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rBluetooth :: Lens.Lens' Radios (Lude.Maybe Lude.Bool)
rBluetooth = Lens.lens (bluetooth :: Radios -> Lude.Maybe Lude.Bool) (\s a -> s {bluetooth = a} :: Radios)
{-# DEPRECATED rBluetooth "Use generic-lens or generic-optics with 'bluetooth' instead." #-}

-- | True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
--
-- /Note:/ Consider using 'wifi' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rWifi :: Lens.Lens' Radios (Lude.Maybe Lude.Bool)
rWifi = Lens.lens (wifi :: Radios -> Lude.Maybe Lude.Bool) (\s a -> s {wifi = a} :: Radios)
{-# DEPRECATED rWifi "Use generic-lens or generic-optics with 'wifi' instead." #-}

instance Lude.FromJSON Radios where
  parseJSON =
    Lude.withObject
      "Radios"
      ( \x ->
          Radios'
            Lude.<$> (x Lude..:? "nfc")
            Lude.<*> (x Lude..:? "gps")
            Lude.<*> (x Lude..:? "bluetooth")
            Lude.<*> (x Lude..:? "wifi")
      )

instance Lude.ToJSON Radios where
  toJSON Radios' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nfc" Lude..=) Lude.<$> nfc,
            ("gps" Lude..=) Lude.<$> gps,
            ("bluetooth" Lude..=) Lude.<$> bluetooth,
            ("wifi" Lude..=) Lude.<$> wifi
          ]
      )
