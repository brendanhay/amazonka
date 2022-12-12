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
-- Module      : Amazonka.QuickSight.Types.BoxPlotAggregatedFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | The aggregated field well for a box plot.
--
-- /See:/ 'newBoxPlotAggregatedFieldWells' smart constructor.
data BoxPlotAggregatedFieldWells = BoxPlotAggregatedFieldWells'
  { -- | The group by field well of a box plot chart. Values are grouped based on
    -- group by fields.
    groupBy :: Prelude.Maybe [DimensionField],
    -- | The value field well of a box plot chart. Values are aggregated based on
    -- group by fields.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoxPlotAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupBy', 'boxPlotAggregatedFieldWells_groupBy' - The group by field well of a box plot chart. Values are grouped based on
-- group by fields.
--
-- 'values', 'boxPlotAggregatedFieldWells_values' - The value field well of a box plot chart. Values are aggregated based on
-- group by fields.
newBoxPlotAggregatedFieldWells ::
  BoxPlotAggregatedFieldWells
newBoxPlotAggregatedFieldWells =
  BoxPlotAggregatedFieldWells'
    { groupBy =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The group by field well of a box plot chart. Values are grouped based on
-- group by fields.
boxPlotAggregatedFieldWells_groupBy :: Lens.Lens' BoxPlotAggregatedFieldWells (Prelude.Maybe [DimensionField])
boxPlotAggregatedFieldWells_groupBy = Lens.lens (\BoxPlotAggregatedFieldWells' {groupBy} -> groupBy) (\s@BoxPlotAggregatedFieldWells' {} a -> s {groupBy = a} :: BoxPlotAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The value field well of a box plot chart. Values are aggregated based on
-- group by fields.
boxPlotAggregatedFieldWells_values :: Lens.Lens' BoxPlotAggregatedFieldWells (Prelude.Maybe [MeasureField])
boxPlotAggregatedFieldWells_values = Lens.lens (\BoxPlotAggregatedFieldWells' {values} -> values) (\s@BoxPlotAggregatedFieldWells' {} a -> s {values = a} :: BoxPlotAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BoxPlotAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "BoxPlotAggregatedFieldWells"
      ( \x ->
          BoxPlotAggregatedFieldWells'
            Prelude.<$> (x Data..:? "GroupBy" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable BoxPlotAggregatedFieldWells where
  hashWithSalt _salt BoxPlotAggregatedFieldWells' {..} =
    _salt `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` values

instance Prelude.NFData BoxPlotAggregatedFieldWells where
  rnf BoxPlotAggregatedFieldWells' {..} =
    Prelude.rnf groupBy
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON BoxPlotAggregatedFieldWells where
  toJSON BoxPlotAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GroupBy" Data..=) Prelude.<$> groupBy,
            ("Values" Data..=) Prelude.<$> values
          ]
      )
