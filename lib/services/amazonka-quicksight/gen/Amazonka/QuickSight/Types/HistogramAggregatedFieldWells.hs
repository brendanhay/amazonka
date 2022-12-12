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
-- Module      : Amazonka.QuickSight.Types.HistogramAggregatedFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HistogramAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.MeasureField

-- | The field well configuration of a histogram.
--
-- /See:/ 'newHistogramAggregatedFieldWells' smart constructor.
data HistogramAggregatedFieldWells = HistogramAggregatedFieldWells'
  { -- | The value field wells of a histogram. Values are aggregated by @COUNT@
    -- or @DISTINCT_COUNT@.
    values :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistogramAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'histogramAggregatedFieldWells_values' - The value field wells of a histogram. Values are aggregated by @COUNT@
-- or @DISTINCT_COUNT@.
newHistogramAggregatedFieldWells ::
  HistogramAggregatedFieldWells
newHistogramAggregatedFieldWells =
  HistogramAggregatedFieldWells'
    { values =
        Prelude.Nothing
    }

-- | The value field wells of a histogram. Values are aggregated by @COUNT@
-- or @DISTINCT_COUNT@.
histogramAggregatedFieldWells_values :: Lens.Lens' HistogramAggregatedFieldWells (Prelude.Maybe [MeasureField])
histogramAggregatedFieldWells_values = Lens.lens (\HistogramAggregatedFieldWells' {values} -> values) (\s@HistogramAggregatedFieldWells' {} a -> s {values = a} :: HistogramAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON HistogramAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "HistogramAggregatedFieldWells"
      ( \x ->
          HistogramAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    HistogramAggregatedFieldWells
  where
  hashWithSalt _salt HistogramAggregatedFieldWells' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData HistogramAggregatedFieldWells where
  rnf HistogramAggregatedFieldWells' {..} =
    Prelude.rnf values

instance Data.ToJSON HistogramAggregatedFieldWells where
  toJSON HistogramAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Values" Data..=) Prelude.<$> values]
      )
