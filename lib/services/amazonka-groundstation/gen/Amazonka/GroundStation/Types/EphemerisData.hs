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
-- Module      : Amazonka.GroundStation.Types.EphemerisData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.EphemerisData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types.OEMEphemeris
import Amazonka.GroundStation.Types.TLEEphemeris
import qualified Amazonka.Prelude as Prelude

-- | Ephemeris data.
--
-- /See:/ 'newEphemerisData' smart constructor.
data EphemerisData = EphemerisData'
  { tle :: Prelude.Maybe TLEEphemeris,
    oem :: Prelude.Maybe OEMEphemeris
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EphemerisData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tle', 'ephemerisData_tle' - Undocumented member.
--
-- 'oem', 'ephemerisData_oem' - Undocumented member.
newEphemerisData ::
  EphemerisData
newEphemerisData =
  EphemerisData'
    { tle = Prelude.Nothing,
      oem = Prelude.Nothing
    }

-- | Undocumented member.
ephemerisData_tle :: Lens.Lens' EphemerisData (Prelude.Maybe TLEEphemeris)
ephemerisData_tle = Lens.lens (\EphemerisData' {tle} -> tle) (\s@EphemerisData' {} a -> s {tle = a} :: EphemerisData)

-- | Undocumented member.
ephemerisData_oem :: Lens.Lens' EphemerisData (Prelude.Maybe OEMEphemeris)
ephemerisData_oem = Lens.lens (\EphemerisData' {oem} -> oem) (\s@EphemerisData' {} a -> s {oem = a} :: EphemerisData)

instance Prelude.Hashable EphemerisData where
  hashWithSalt _salt EphemerisData' {..} =
    _salt `Prelude.hashWithSalt` tle
      `Prelude.hashWithSalt` oem

instance Prelude.NFData EphemerisData where
  rnf EphemerisData' {..} =
    Prelude.rnf tle `Prelude.seq` Prelude.rnf oem

instance Core.ToJSON EphemerisData where
  toJSON EphemerisData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tle" Core..=) Prelude.<$> tle,
            ("oem" Core..=) Prelude.<$> oem
          ]
      )
