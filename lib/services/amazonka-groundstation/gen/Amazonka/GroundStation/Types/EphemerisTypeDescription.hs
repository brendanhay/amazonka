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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisTypeDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.EphemerisDescription
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newEphemerisTypeDescription' smart constructor.
data EphemerisTypeDescription = EphemerisTypeDescription'
  { oem :: Prelude.Maybe EphemerisDescription,
    tle :: Prelude.Maybe EphemerisDescription
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
-- 'oem', 'ephemerisTypeDescription_oem' - Undocumented member.
--
-- 'tle', 'ephemerisTypeDescription_tle' - Undocumented member.
newEphemerisTypeDescription ::
  EphemerisTypeDescription
newEphemerisTypeDescription =
  EphemerisTypeDescription'
    { oem = Prelude.Nothing,
      tle = Prelude.Nothing
    }

-- | Undocumented member.
ephemerisTypeDescription_oem :: Lens.Lens' EphemerisTypeDescription (Prelude.Maybe EphemerisDescription)
ephemerisTypeDescription_oem = Lens.lens (\EphemerisTypeDescription' {oem} -> oem) (\s@EphemerisTypeDescription' {} a -> s {oem = a} :: EphemerisTypeDescription)

-- | Undocumented member.
ephemerisTypeDescription_tle :: Lens.Lens' EphemerisTypeDescription (Prelude.Maybe EphemerisDescription)
ephemerisTypeDescription_tle = Lens.lens (\EphemerisTypeDescription' {tle} -> tle) (\s@EphemerisTypeDescription' {} a -> s {tle = a} :: EphemerisTypeDescription)

instance Data.FromJSON EphemerisTypeDescription where
  parseJSON =
    Data.withObject
      "EphemerisTypeDescription"
      ( \x ->
          EphemerisTypeDescription'
            Prelude.<$> (x Data..:? "oem") Prelude.<*> (x Data..:? "tle")
      )

instance Prelude.Hashable EphemerisTypeDescription where
  hashWithSalt _salt EphemerisTypeDescription' {..} =
    _salt `Prelude.hashWithSalt` oem
      `Prelude.hashWithSalt` tle

instance Prelude.NFData EphemerisTypeDescription where
  rnf EphemerisTypeDescription' {..} =
    Prelude.rnf oem `Prelude.seq` Prelude.rnf tle
