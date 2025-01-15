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
-- Module      : Amazonka.QuickSight.Types.KPIFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a KPI visual.
--
-- /See:/ 'newKPIFieldWells' smart constructor.
data KPIFieldWells = KPIFieldWells'
  { -- | The target value field wells of a KPI visual.
    targetValues :: Prelude.Maybe [MeasureField],
    -- | The trend group field wells of a KPI visual.
    trendGroups :: Prelude.Maybe [DimensionField],
    -- | The value field wells of a KPI visual.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetValues', 'kPIFieldWells_targetValues' - The target value field wells of a KPI visual.
--
-- 'trendGroups', 'kPIFieldWells_trendGroups' - The trend group field wells of a KPI visual.
--
-- 'values', 'kPIFieldWells_values' - The value field wells of a KPI visual.
newKPIFieldWells ::
  KPIFieldWells
newKPIFieldWells =
  KPIFieldWells'
    { targetValues = Prelude.Nothing,
      trendGroups = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The target value field wells of a KPI visual.
kPIFieldWells_targetValues :: Lens.Lens' KPIFieldWells (Prelude.Maybe [MeasureField])
kPIFieldWells_targetValues = Lens.lens (\KPIFieldWells' {targetValues} -> targetValues) (\s@KPIFieldWells' {} a -> s {targetValues = a} :: KPIFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The trend group field wells of a KPI visual.
kPIFieldWells_trendGroups :: Lens.Lens' KPIFieldWells (Prelude.Maybe [DimensionField])
kPIFieldWells_trendGroups = Lens.lens (\KPIFieldWells' {trendGroups} -> trendGroups) (\s@KPIFieldWells' {} a -> s {trendGroups = a} :: KPIFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field wells of a KPI visual.
kPIFieldWells_values :: Lens.Lens' KPIFieldWells (Prelude.Maybe [MeasureField])
kPIFieldWells_values = Lens.lens (\KPIFieldWells' {values} -> values) (\s@KPIFieldWells' {} a -> s {values = a} :: KPIFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON KPIFieldWells where
  parseJSON =
    Data.withObject
      "KPIFieldWells"
      ( \x ->
          KPIFieldWells'
            Prelude.<$> (x Data..:? "TargetValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TrendGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable KPIFieldWells where
  hashWithSalt _salt KPIFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` targetValues
      `Prelude.hashWithSalt` trendGroups
      `Prelude.hashWithSalt` values

instance Prelude.NFData KPIFieldWells where
  rnf KPIFieldWells' {..} =
    Prelude.rnf targetValues `Prelude.seq`
      Prelude.rnf trendGroups `Prelude.seq`
        Prelude.rnf values

instance Data.ToJSON KPIFieldWells where
  toJSON KPIFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TargetValues" Data..=) Prelude.<$> targetValues,
            ("TrendGroups" Data..=) Prelude.<$> trendGroups,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
