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
-- Module      : Amazonka.QuickSight.Types.GaugeChartFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GaugeChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a @GaugeChartVisual@.
--
-- /See:/ 'newGaugeChartFieldWells' smart constructor.
data GaugeChartFieldWells = GaugeChartFieldWells'
  { -- | The target value field wells of a @GaugeChartVisual@.
    targetValues :: Prelude.Maybe [MeasureField],
    -- | The value field wells of a @GaugeChartVisual@.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GaugeChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetValues', 'gaugeChartFieldWells_targetValues' - The target value field wells of a @GaugeChartVisual@.
--
-- 'values', 'gaugeChartFieldWells_values' - The value field wells of a @GaugeChartVisual@.
newGaugeChartFieldWells ::
  GaugeChartFieldWells
newGaugeChartFieldWells =
  GaugeChartFieldWells'
    { targetValues =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The target value field wells of a @GaugeChartVisual@.
gaugeChartFieldWells_targetValues :: Lens.Lens' GaugeChartFieldWells (Prelude.Maybe [MeasureField])
gaugeChartFieldWells_targetValues = Lens.lens (\GaugeChartFieldWells' {targetValues} -> targetValues) (\s@GaugeChartFieldWells' {} a -> s {targetValues = a} :: GaugeChartFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field wells of a @GaugeChartVisual@.
gaugeChartFieldWells_values :: Lens.Lens' GaugeChartFieldWells (Prelude.Maybe [MeasureField])
gaugeChartFieldWells_values = Lens.lens (\GaugeChartFieldWells' {values} -> values) (\s@GaugeChartFieldWells' {} a -> s {values = a} :: GaugeChartFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON GaugeChartFieldWells where
  parseJSON =
    Data.withObject
      "GaugeChartFieldWells"
      ( \x ->
          GaugeChartFieldWells'
            Prelude.<$> (x Data..:? "TargetValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable GaugeChartFieldWells where
  hashWithSalt _salt GaugeChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` targetValues
      `Prelude.hashWithSalt` values

instance Prelude.NFData GaugeChartFieldWells where
  rnf GaugeChartFieldWells' {..} =
    Prelude.rnf targetValues
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON GaugeChartFieldWells where
  toJSON GaugeChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetValues" Data..=) Prelude.<$> targetValues,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
