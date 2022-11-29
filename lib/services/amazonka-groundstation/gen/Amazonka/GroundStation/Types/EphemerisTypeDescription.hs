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
-- Module      : Amazonka.GroundStation.Types.EphemerisTypeDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisTypeDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.EphemerisDescription
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newEphemerisTypeDescription' smart constructor.
data EphemerisTypeDescription = EphemerisTypeDescription'
  { tle :: Prelude.Maybe EphemerisDescription,
    oem :: Prelude.Maybe EphemerisDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EphemerisTypeDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tle', 'ephemerisTypeDescription_tle' - Undocumented member.
--
-- 'oem', 'ephemerisTypeDescription_oem' - Undocumented member.
newEphemerisTypeDescription ::
  EphemerisTypeDescription
newEphemerisTypeDescription =
  EphemerisTypeDescription'
    { tle = Prelude.Nothing,
      oem = Prelude.Nothing
    }

-- | Undocumented member.
ephemerisTypeDescription_tle :: Lens.Lens' EphemerisTypeDescription (Prelude.Maybe EphemerisDescription)
ephemerisTypeDescription_tle = Lens.lens (\EphemerisTypeDescription' {tle} -> tle) (\s@EphemerisTypeDescription' {} a -> s {tle = a} :: EphemerisTypeDescription)

-- | Undocumented member.
ephemerisTypeDescription_oem :: Lens.Lens' EphemerisTypeDescription (Prelude.Maybe EphemerisDescription)
ephemerisTypeDescription_oem = Lens.lens (\EphemerisTypeDescription' {oem} -> oem) (\s@EphemerisTypeDescription' {} a -> s {oem = a} :: EphemerisTypeDescription)

instance Core.FromJSON EphemerisTypeDescription where
  parseJSON =
    Core.withObject
      "EphemerisTypeDescription"
      ( \x ->
          EphemerisTypeDescription'
            Prelude.<$> (x Core..:? "tle") Prelude.<*> (x Core..:? "oem")
      )

instance Prelude.Hashable EphemerisTypeDescription where
  hashWithSalt _salt EphemerisTypeDescription' {..} =
    _salt `Prelude.hashWithSalt` tle
      `Prelude.hashWithSalt` oem

instance Prelude.NFData EphemerisTypeDescription where
  rnf EphemerisTypeDescription' {..} =
    Prelude.rnf tle `Prelude.seq` Prelude.rnf oem
