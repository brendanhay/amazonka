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
-- Module      : Amazonka.QuickSight.Types.WaterfallChartFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WaterfallChartFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.WaterfallChartAggregatedFieldWells

-- | The field well configuration of a waterfall visual.
--
-- /See:/ 'newWaterfallChartFieldWells' smart constructor.
data WaterfallChartFieldWells = WaterfallChartFieldWells'
  { -- | The field well configuration of a waterfall visual.
    waterfallChartAggregatedFieldWells :: Prelude.Maybe WaterfallChartAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaterfallChartFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'waterfallChartAggregatedFieldWells', 'waterfallChartFieldWells_waterfallChartAggregatedFieldWells' - The field well configuration of a waterfall visual.
newWaterfallChartFieldWells ::
  WaterfallChartFieldWells
newWaterfallChartFieldWells =
  WaterfallChartFieldWells'
    { waterfallChartAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The field well configuration of a waterfall visual.
waterfallChartFieldWells_waterfallChartAggregatedFieldWells :: Lens.Lens' WaterfallChartFieldWells (Prelude.Maybe WaterfallChartAggregatedFieldWells)
waterfallChartFieldWells_waterfallChartAggregatedFieldWells = Lens.lens (\WaterfallChartFieldWells' {waterfallChartAggregatedFieldWells} -> waterfallChartAggregatedFieldWells) (\s@WaterfallChartFieldWells' {} a -> s {waterfallChartAggregatedFieldWells = a} :: WaterfallChartFieldWells)

instance Data.FromJSON WaterfallChartFieldWells where
  parseJSON =
    Data.withObject
      "WaterfallChartFieldWells"
      ( \x ->
          WaterfallChartFieldWells'
            Prelude.<$> (x Data..:? "WaterfallChartAggregatedFieldWells")
      )

instance Prelude.Hashable WaterfallChartFieldWells where
  hashWithSalt _salt WaterfallChartFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` waterfallChartAggregatedFieldWells

instance Prelude.NFData WaterfallChartFieldWells where
  rnf WaterfallChartFieldWells' {..} =
    Prelude.rnf waterfallChartAggregatedFieldWells

instance Data.ToJSON WaterfallChartFieldWells where
  toJSON WaterfallChartFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WaterfallChartAggregatedFieldWells" Data..=)
              Prelude.<$> waterfallChartAggregatedFieldWells
          ]
      )
