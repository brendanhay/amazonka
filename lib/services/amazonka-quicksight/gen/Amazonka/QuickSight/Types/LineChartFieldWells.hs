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
-- Module      : Amazonka.QuickSight.Types.LineChartFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LineChartAggregatedFieldWells

-- | The field well configuration of a line chart.
--
-- /See:/ 'newLineChartFieldWells' smart constructor.
data LineChartFieldWells = LineChartFieldWells'
  { -- | The field well configuration of a line chart.
    lineChartAggregatedFieldWells :: Prelude.Maybe LineChartAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineChartAggregatedFieldWells', 'lineChartFieldWells_lineChartAggregatedFieldWells' - The field well configuration of a line chart.
newLineChartFieldWells ::
  LineChartFieldWells
newLineChartFieldWells =
  LineChartFieldWells'
    { lineChartAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The field well configuration of a line chart.
lineChartFieldWells_lineChartAggregatedFieldWells :: Lens.Lens' LineChartFieldWells (Prelude.Maybe LineChartAggregatedFieldWells)
lineChartFieldWells_lineChartAggregatedFieldWells = Lens.lens (\LineChartFieldWells' {lineChartAggregatedFieldWells} -> lineChartAggregatedFieldWells) (\s@LineChartFieldWells' {} a -> s {lineChartAggregatedFieldWells = a} :: LineChartFieldWells)

instance Data.FromJSON LineChartFieldWells where
  parseJSON =
    Data.withObject
      "LineChartFieldWells"
      ( \x ->
          LineChartFieldWells'
            Prelude.<$> (x Data..:? "LineChartAggregatedFieldWells")
      )

instance Prelude.Hashable LineChartFieldWells where
  hashWithSalt _salt LineChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` lineChartAggregatedFieldWells

instance Prelude.NFData LineChartFieldWells where
  rnf LineChartFieldWells' {..} =
    Prelude.rnf lineChartAggregatedFieldWells

instance Data.ToJSON LineChartFieldWells where
  toJSON LineChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LineChartAggregatedFieldWells" Data..=)
              Prelude.<$> lineChartAggregatedFieldWells
          ]
      )
