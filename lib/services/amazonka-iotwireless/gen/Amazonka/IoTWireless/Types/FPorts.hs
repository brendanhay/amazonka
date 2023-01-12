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
-- Module      : Amazonka.IoTWireless.Types.FPorts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.FPorts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.ApplicationConfig
import Amazonka.IoTWireless.Types.Positioning
import qualified Amazonka.Prelude as Prelude

-- | List of FPort assigned for different LoRaWAN application packages to use
--
-- /See:/ 'newFPorts' smart constructor.
data FPorts = FPorts'
  { -- | Optional LoRaWAN application information, which can be used for
    -- geolocation.
    applications :: Prelude.Maybe [ApplicationConfig],
    clockSync :: Prelude.Maybe Prelude.Natural,
    fuota :: Prelude.Maybe Prelude.Natural,
    multicast :: Prelude.Maybe Prelude.Natural,
    -- | FPort values for the GNSS, stream, and ClockSync functions of the
    -- positioning information.
    positioning :: Prelude.Maybe Positioning
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FPorts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applications', 'fPorts_applications' - Optional LoRaWAN application information, which can be used for
-- geolocation.
--
-- 'clockSync', 'fPorts_clockSync' - Undocumented member.
--
-- 'fuota', 'fPorts_fuota' - Undocumented member.
--
-- 'multicast', 'fPorts_multicast' - Undocumented member.
--
-- 'positioning', 'fPorts_positioning' - FPort values for the GNSS, stream, and ClockSync functions of the
-- positioning information.
newFPorts ::
  FPorts
newFPorts =
  FPorts'
    { applications = Prelude.Nothing,
      clockSync = Prelude.Nothing,
      fuota = Prelude.Nothing,
      multicast = Prelude.Nothing,
      positioning = Prelude.Nothing
    }

-- | Optional LoRaWAN application information, which can be used for
-- geolocation.
fPorts_applications :: Lens.Lens' FPorts (Prelude.Maybe [ApplicationConfig])
fPorts_applications = Lens.lens (\FPorts' {applications} -> applications) (\s@FPorts' {} a -> s {applications = a} :: FPorts) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
fPorts_clockSync :: Lens.Lens' FPorts (Prelude.Maybe Prelude.Natural)
fPorts_clockSync = Lens.lens (\FPorts' {clockSync} -> clockSync) (\s@FPorts' {} a -> s {clockSync = a} :: FPorts)

-- | Undocumented member.
fPorts_fuota :: Lens.Lens' FPorts (Prelude.Maybe Prelude.Natural)
fPorts_fuota = Lens.lens (\FPorts' {fuota} -> fuota) (\s@FPorts' {} a -> s {fuota = a} :: FPorts)

-- | Undocumented member.
fPorts_multicast :: Lens.Lens' FPorts (Prelude.Maybe Prelude.Natural)
fPorts_multicast = Lens.lens (\FPorts' {multicast} -> multicast) (\s@FPorts' {} a -> s {multicast = a} :: FPorts)

-- | FPort values for the GNSS, stream, and ClockSync functions of the
-- positioning information.
fPorts_positioning :: Lens.Lens' FPorts (Prelude.Maybe Positioning)
fPorts_positioning = Lens.lens (\FPorts' {positioning} -> positioning) (\s@FPorts' {} a -> s {positioning = a} :: FPorts)

instance Data.FromJSON FPorts where
  parseJSON =
    Data.withObject
      "FPorts"
      ( \x ->
          FPorts'
            Prelude.<$> (x Data..:? "Applications" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ClockSync")
            Prelude.<*> (x Data..:? "Fuota")
            Prelude.<*> (x Data..:? "Multicast")
            Prelude.<*> (x Data..:? "Positioning")
      )

instance Prelude.Hashable FPorts where
  hashWithSalt _salt FPorts' {..} =
    _salt `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` clockSync
      `Prelude.hashWithSalt` fuota
      `Prelude.hashWithSalt` multicast
      `Prelude.hashWithSalt` positioning

instance Prelude.NFData FPorts where
  rnf FPorts' {..} =
    Prelude.rnf applications
      `Prelude.seq` Prelude.rnf clockSync
      `Prelude.seq` Prelude.rnf fuota
      `Prelude.seq` Prelude.rnf multicast
      `Prelude.seq` Prelude.rnf positioning

instance Data.ToJSON FPorts where
  toJSON FPorts' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Applications" Data..=) Prelude.<$> applications,
            ("ClockSync" Data..=) Prelude.<$> clockSync,
            ("Fuota" Data..=) Prelude.<$> fuota,
            ("Multicast" Data..=) Prelude.<$> multicast,
            ("Positioning" Data..=) Prelude.<$> positioning
          ]
      )
