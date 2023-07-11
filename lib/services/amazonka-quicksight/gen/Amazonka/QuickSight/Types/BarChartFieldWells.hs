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
-- Module      : Amazonka.QuickSight.Types.BarChartFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BarChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BarChartAggregatedFieldWells

-- | The field wells of a @BarChartVisual@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newBarChartFieldWells' smart constructor.
data BarChartFieldWells = BarChartFieldWells'
  { -- | The aggregated field wells of a bar chart.
    barChartAggregatedFieldWells :: Prelude.Maybe BarChartAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BarChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'barChartAggregatedFieldWells', 'barChartFieldWells_barChartAggregatedFieldWells' - The aggregated field wells of a bar chart.
newBarChartFieldWells ::
  BarChartFieldWells
newBarChartFieldWells =
  BarChartFieldWells'
    { barChartAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field wells of a bar chart.
barChartFieldWells_barChartAggregatedFieldWells :: Lens.Lens' BarChartFieldWells (Prelude.Maybe BarChartAggregatedFieldWells)
barChartFieldWells_barChartAggregatedFieldWells = Lens.lens (\BarChartFieldWells' {barChartAggregatedFieldWells} -> barChartAggregatedFieldWells) (\s@BarChartFieldWells' {} a -> s {barChartAggregatedFieldWells = a} :: BarChartFieldWells)

instance Data.FromJSON BarChartFieldWells where
  parseJSON =
    Data.withObject
      "BarChartFieldWells"
      ( \x ->
          BarChartFieldWells'
            Prelude.<$> (x Data..:? "BarChartAggregatedFieldWells")
      )

instance Prelude.Hashable BarChartFieldWells where
  hashWithSalt _salt BarChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` barChartAggregatedFieldWells

instance Prelude.NFData BarChartFieldWells where
  rnf BarChartFieldWells' {..} =
    Prelude.rnf barChartAggregatedFieldWells

instance Data.ToJSON BarChartFieldWells where
  toJSON BarChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BarChartAggregatedFieldWells" Data..=)
              Prelude.<$> barChartAggregatedFieldWells
          ]
      )
