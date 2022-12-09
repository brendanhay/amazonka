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
-- Module      : Amazonka.QuickSight.Types.ComboChartFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComboChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ComboChartAggregatedFieldWells

-- | The field wells of the visual.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newComboChartFieldWells' smart constructor.
data ComboChartFieldWells = ComboChartFieldWells'
  { -- | The aggregated field wells of a combo chart. Combo charts only have
    -- aggregated field wells. Columns in a combo chart are aggregated by
    -- category.
    comboChartAggregatedFieldWells :: Prelude.Maybe ComboChartAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComboChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comboChartAggregatedFieldWells', 'comboChartFieldWells_comboChartAggregatedFieldWells' - The aggregated field wells of a combo chart. Combo charts only have
-- aggregated field wells. Columns in a combo chart are aggregated by
-- category.
newComboChartFieldWells ::
  ComboChartFieldWells
newComboChartFieldWells =
  ComboChartFieldWells'
    { comboChartAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field wells of a combo chart. Combo charts only have
-- aggregated field wells. Columns in a combo chart are aggregated by
-- category.
comboChartFieldWells_comboChartAggregatedFieldWells :: Lens.Lens' ComboChartFieldWells (Prelude.Maybe ComboChartAggregatedFieldWells)
comboChartFieldWells_comboChartAggregatedFieldWells = Lens.lens (\ComboChartFieldWells' {comboChartAggregatedFieldWells} -> comboChartAggregatedFieldWells) (\s@ComboChartFieldWells' {} a -> s {comboChartAggregatedFieldWells = a} :: ComboChartFieldWells)

instance Data.FromJSON ComboChartFieldWells where
  parseJSON =
    Data.withObject
      "ComboChartFieldWells"
      ( \x ->
          ComboChartFieldWells'
            Prelude.<$> (x Data..:? "ComboChartAggregatedFieldWells")
      )

instance Prelude.Hashable ComboChartFieldWells where
  hashWithSalt _salt ComboChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` comboChartAggregatedFieldWells

instance Prelude.NFData ComboChartFieldWells where
  rnf ComboChartFieldWells' {..} =
    Prelude.rnf comboChartAggregatedFieldWells

instance Data.ToJSON ComboChartFieldWells where
  toJSON ComboChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComboChartAggregatedFieldWells" Data..=)
              Prelude.<$> comboChartAggregatedFieldWells
          ]
      )
