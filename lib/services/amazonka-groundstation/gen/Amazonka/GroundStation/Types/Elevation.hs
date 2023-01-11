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
-- Module      : Amazonka.GroundStation.Types.Elevation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.Elevation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.AngleUnits
import qualified Amazonka.Prelude as Prelude

-- | Elevation angle of the satellite in the sky during a contact.
--
-- /See:/ 'newElevation' smart constructor.
data Elevation = Elevation'
  { -- | Elevation angle units.
    unit :: AngleUnits,
    -- | Elevation angle value.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Elevation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'elevation_unit' - Elevation angle units.
--
-- 'value', 'elevation_value' - Elevation angle value.
newElevation ::
  -- | 'unit'
  AngleUnits ->
  -- | 'value'
  Prelude.Double ->
  Elevation
newElevation pUnit_ pValue_ =
  Elevation' {unit = pUnit_, value = pValue_}

-- | Elevation angle units.
elevation_unit :: Lens.Lens' Elevation AngleUnits
elevation_unit = Lens.lens (\Elevation' {unit} -> unit) (\s@Elevation' {} a -> s {unit = a} :: Elevation)

-- | Elevation angle value.
elevation_value :: Lens.Lens' Elevation Prelude.Double
elevation_value = Lens.lens (\Elevation' {value} -> value) (\s@Elevation' {} a -> s {value = a} :: Elevation)

instance Data.FromJSON Elevation where
  parseJSON =
    Data.withObject
      "Elevation"
      ( \x ->
          Elevation'
            Prelude.<$> (x Data..: "unit") Prelude.<*> (x Data..: "value")
      )

instance Prelude.Hashable Elevation where
  hashWithSalt _salt Elevation' {..} =
    _salt `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData Elevation where
  rnf Elevation' {..} =
    Prelude.rnf unit `Prelude.seq` Prelude.rnf value
