{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Radios
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Radios where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the set of radios and their states on a device. Examples of
-- radios include Wi-Fi, GPS, Bluetooth, and NFC.
--
-- /See:/ 'newRadios' smart constructor.
data Radios = Radios'
  { -- | True if GPS is enabled at the beginning of the test. Otherwise, false.
    gps :: Prelude.Maybe Prelude.Bool,
    -- | True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
    wifi :: Prelude.Maybe Prelude.Bool,
    -- | True if Bluetooth is enabled at the beginning of the test. Otherwise,
    -- false.
    bluetooth :: Prelude.Maybe Prelude.Bool,
    -- | True if NFC is enabled at the beginning of the test. Otherwise, false.
    nfc :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Radios' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gps', 'radios_gps' - True if GPS is enabled at the beginning of the test. Otherwise, false.
--
-- 'wifi', 'radios_wifi' - True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
--
-- 'bluetooth', 'radios_bluetooth' - True if Bluetooth is enabled at the beginning of the test. Otherwise,
-- false.
--
-- 'nfc', 'radios_nfc' - True if NFC is enabled at the beginning of the test. Otherwise, false.
newRadios ::
  Radios
newRadios =
  Radios'
    { gps = Prelude.Nothing,
      wifi = Prelude.Nothing,
      bluetooth = Prelude.Nothing,
      nfc = Prelude.Nothing
    }

-- | True if GPS is enabled at the beginning of the test. Otherwise, false.
radios_gps :: Lens.Lens' Radios (Prelude.Maybe Prelude.Bool)
radios_gps = Lens.lens (\Radios' {gps} -> gps) (\s@Radios' {} a -> s {gps = a} :: Radios)

-- | True if Wi-Fi is enabled at the beginning of the test. Otherwise, false.
radios_wifi :: Lens.Lens' Radios (Prelude.Maybe Prelude.Bool)
radios_wifi = Lens.lens (\Radios' {wifi} -> wifi) (\s@Radios' {} a -> s {wifi = a} :: Radios)

-- | True if Bluetooth is enabled at the beginning of the test. Otherwise,
-- false.
radios_bluetooth :: Lens.Lens' Radios (Prelude.Maybe Prelude.Bool)
radios_bluetooth = Lens.lens (\Radios' {bluetooth} -> bluetooth) (\s@Radios' {} a -> s {bluetooth = a} :: Radios)

-- | True if NFC is enabled at the beginning of the test. Otherwise, false.
radios_nfc :: Lens.Lens' Radios (Prelude.Maybe Prelude.Bool)
radios_nfc = Lens.lens (\Radios' {nfc} -> nfc) (\s@Radios' {} a -> s {nfc = a} :: Radios)

instance Prelude.FromJSON Radios where
  parseJSON =
    Prelude.withObject
      "Radios"
      ( \x ->
          Radios'
            Prelude.<$> (x Prelude..:? "gps")
            Prelude.<*> (x Prelude..:? "wifi")
            Prelude.<*> (x Prelude..:? "bluetooth")
            Prelude.<*> (x Prelude..:? "nfc")
      )

instance Prelude.Hashable Radios

instance Prelude.NFData Radios

instance Prelude.ToJSON Radios where
  toJSON Radios' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("gps" Prelude..=) Prelude.<$> gps,
            ("wifi" Prelude..=) Prelude.<$> wifi,
            ("bluetooth" Prelude..=) Prelude.<$> bluetooth,
            ("nfc" Prelude..=) Prelude.<$> nfc
          ]
      )
