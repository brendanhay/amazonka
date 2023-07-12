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
-- Module      : Amazonka.QuickSight.Types.ArcAxisConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ArcAxisConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ArcAxisDisplayRange

-- | The arc axis configuration of a @GaugeChartVisual@.
--
-- /See:/ 'newArcAxisConfiguration' smart constructor.
data ArcAxisConfiguration = ArcAxisConfiguration'
  { -- | The arc axis range of a @GaugeChartVisual@.
    range :: Prelude.Maybe ArcAxisDisplayRange,
    -- | The reserved range of the arc axis.
    reserveRange :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArcAxisConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'range', 'arcAxisConfiguration_range' - The arc axis range of a @GaugeChartVisual@.
--
-- 'reserveRange', 'arcAxisConfiguration_reserveRange' - The reserved range of the arc axis.
newArcAxisConfiguration ::
  ArcAxisConfiguration
newArcAxisConfiguration =
  ArcAxisConfiguration'
    { range = Prelude.Nothing,
      reserveRange = Prelude.Nothing
    }

-- | The arc axis range of a @GaugeChartVisual@.
arcAxisConfiguration_range :: Lens.Lens' ArcAxisConfiguration (Prelude.Maybe ArcAxisDisplayRange)
arcAxisConfiguration_range = Lens.lens (\ArcAxisConfiguration' {range} -> range) (\s@ArcAxisConfiguration' {} a -> s {range = a} :: ArcAxisConfiguration)

-- | The reserved range of the arc axis.
arcAxisConfiguration_reserveRange :: Lens.Lens' ArcAxisConfiguration (Prelude.Maybe Prelude.Int)
arcAxisConfiguration_reserveRange = Lens.lens (\ArcAxisConfiguration' {reserveRange} -> reserveRange) (\s@ArcAxisConfiguration' {} a -> s {reserveRange = a} :: ArcAxisConfiguration)

instance Data.FromJSON ArcAxisConfiguration where
  parseJSON =
    Data.withObject
      "ArcAxisConfiguration"
      ( \x ->
          ArcAxisConfiguration'
            Prelude.<$> (x Data..:? "Range")
            Prelude.<*> (x Data..:? "ReserveRange")
      )

instance Prelude.Hashable ArcAxisConfiguration where
  hashWithSalt _salt ArcAxisConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` range
      `Prelude.hashWithSalt` reserveRange

instance Prelude.NFData ArcAxisConfiguration where
  rnf ArcAxisConfiguration' {..} =
    Prelude.rnf range
      `Prelude.seq` Prelude.rnf reserveRange

instance Data.ToJSON ArcAxisConfiguration where
  toJSON ArcAxisConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Range" Data..=) Prelude.<$> range,
            ("ReserveRange" Data..=) Prelude.<$> reserveRange
          ]
      )
