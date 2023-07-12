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
-- Module      : Amazonka.QuickSight.Types.PieChartFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PieChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PieChartAggregatedFieldWells

-- | The field well configuration of a pie chart.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newPieChartFieldWells' smart constructor.
data PieChartFieldWells = PieChartFieldWells'
  { -- | The field well configuration of a pie chart.
    pieChartAggregatedFieldWells :: Prelude.Maybe PieChartAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PieChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pieChartAggregatedFieldWells', 'pieChartFieldWells_pieChartAggregatedFieldWells' - The field well configuration of a pie chart.
newPieChartFieldWells ::
  PieChartFieldWells
newPieChartFieldWells =
  PieChartFieldWells'
    { pieChartAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The field well configuration of a pie chart.
pieChartFieldWells_pieChartAggregatedFieldWells :: Lens.Lens' PieChartFieldWells (Prelude.Maybe PieChartAggregatedFieldWells)
pieChartFieldWells_pieChartAggregatedFieldWells = Lens.lens (\PieChartFieldWells' {pieChartAggregatedFieldWells} -> pieChartAggregatedFieldWells) (\s@PieChartFieldWells' {} a -> s {pieChartAggregatedFieldWells = a} :: PieChartFieldWells)

instance Data.FromJSON PieChartFieldWells where
  parseJSON =
    Data.withObject
      "PieChartFieldWells"
      ( \x ->
          PieChartFieldWells'
            Prelude.<$> (x Data..:? "PieChartAggregatedFieldWells")
      )

instance Prelude.Hashable PieChartFieldWells where
  hashWithSalt _salt PieChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` pieChartAggregatedFieldWells

instance Prelude.NFData PieChartFieldWells where
  rnf PieChartFieldWells' {..} =
    Prelude.rnf pieChartAggregatedFieldWells

instance Data.ToJSON PieChartFieldWells where
  toJSON PieChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PieChartAggregatedFieldWells" Data..=)
              Prelude.<$> pieChartAggregatedFieldWells
          ]
      )
