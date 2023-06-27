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
-- Module      : Amazonka.IoTWireless.Types.Positioning
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.Positioning where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The FPorts for the position information.
--
-- /See:/ 'newPositioning' smart constructor.
data Positioning = Positioning'
  { clockSync :: Prelude.Maybe Prelude.Natural,
    gnss :: Prelude.Maybe Prelude.Natural,
    stream :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Positioning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clockSync', 'positioning_clockSync' - Undocumented member.
--
-- 'gnss', 'positioning_gnss' - Undocumented member.
--
-- 'stream', 'positioning_stream' - Undocumented member.
newPositioning ::
  Positioning
newPositioning =
  Positioning'
    { clockSync = Prelude.Nothing,
      gnss = Prelude.Nothing,
      stream = Prelude.Nothing
    }

-- | Undocumented member.
positioning_clockSync :: Lens.Lens' Positioning (Prelude.Maybe Prelude.Natural)
positioning_clockSync = Lens.lens (\Positioning' {clockSync} -> clockSync) (\s@Positioning' {} a -> s {clockSync = a} :: Positioning)

-- | Undocumented member.
positioning_gnss :: Lens.Lens' Positioning (Prelude.Maybe Prelude.Natural)
positioning_gnss = Lens.lens (\Positioning' {gnss} -> gnss) (\s@Positioning' {} a -> s {gnss = a} :: Positioning)

-- | Undocumented member.
positioning_stream :: Lens.Lens' Positioning (Prelude.Maybe Prelude.Natural)
positioning_stream = Lens.lens (\Positioning' {stream} -> stream) (\s@Positioning' {} a -> s {stream = a} :: Positioning)

instance Data.FromJSON Positioning where
  parseJSON =
    Data.withObject
      "Positioning"
      ( \x ->
          Positioning'
            Prelude.<$> (x Data..:? "ClockSync")
            Prelude.<*> (x Data..:? "Gnss")
            Prelude.<*> (x Data..:? "Stream")
      )

instance Prelude.Hashable Positioning where
  hashWithSalt _salt Positioning' {..} =
    _salt
      `Prelude.hashWithSalt` clockSync
      `Prelude.hashWithSalt` gnss
      `Prelude.hashWithSalt` stream

instance Prelude.NFData Positioning where
  rnf Positioning' {..} =
    Prelude.rnf clockSync
      `Prelude.seq` Prelude.rnf gnss
      `Prelude.seq` Prelude.rnf stream

instance Data.ToJSON Positioning where
  toJSON Positioning' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClockSync" Data..=) Prelude.<$> clockSync,
            ("Gnss" Data..=) Prelude.<$> gnss,
            ("Stream" Data..=) Prelude.<$> stream
          ]
      )
