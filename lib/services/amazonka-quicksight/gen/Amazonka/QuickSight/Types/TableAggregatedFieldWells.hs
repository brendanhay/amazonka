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
-- Module      : Amazonka.QuickSight.Types.TableAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field well for the table.
--
-- /See:/ 'newTableAggregatedFieldWells' smart constructor.
data TableAggregatedFieldWells = TableAggregatedFieldWells'
  { -- | The group by field well for a pivot table. Values are grouped by group
    -- by fields.
    groupBy :: Prelude.Maybe [DimensionField],
    -- | The values field well for a pivot table. Values are aggregated based on
    -- group by fields.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupBy', 'tableAggregatedFieldWells_groupBy' - The group by field well for a pivot table. Values are grouped by group
-- by fields.
--
-- 'values', 'tableAggregatedFieldWells_values' - The values field well for a pivot table. Values are aggregated based on
-- group by fields.
newTableAggregatedFieldWells ::
  TableAggregatedFieldWells
newTableAggregatedFieldWells =
  TableAggregatedFieldWells'
    { groupBy =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The group by field well for a pivot table. Values are grouped by group
-- by fields.
tableAggregatedFieldWells_groupBy :: Lens.Lens' TableAggregatedFieldWells (Prelude.Maybe [DimensionField])
tableAggregatedFieldWells_groupBy = Lens.lens (\TableAggregatedFieldWells' {groupBy} -> groupBy) (\s@TableAggregatedFieldWells' {} a -> s {groupBy = a} :: TableAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The values field well for a pivot table. Values are aggregated based on
-- group by fields.
tableAggregatedFieldWells_values :: Lens.Lens' TableAggregatedFieldWells (Prelude.Maybe [MeasureField])
tableAggregatedFieldWells_values = Lens.lens (\TableAggregatedFieldWells' {values} -> values) (\s@TableAggregatedFieldWells' {} a -> s {values = a} :: TableAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TableAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "TableAggregatedFieldWells"
      ( \x ->
          TableAggregatedFieldWells'
            Prelude.<$> (x Data..:? "GroupBy" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TableAggregatedFieldWells where
  hashWithSalt _salt TableAggregatedFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` values

instance Prelude.NFData TableAggregatedFieldWells where
  rnf TableAggregatedFieldWells' {..} =
    Prelude.rnf groupBy `Prelude.seq`
      Prelude.rnf values

instance Data.ToJSON TableAggregatedFieldWells where
  toJSON TableAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupBy" Data..=) Prelude.<$> groupBy,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
