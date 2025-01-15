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
-- Module      : Amazonka.QuickSight.Types.WaterfallChartSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WaterfallChartSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a waterfall visual.
--
-- /See:/ 'newWaterfallChartSortConfiguration' smart constructor.
data WaterfallChartSortConfiguration = WaterfallChartSortConfiguration'
  { -- | The limit on the number of bar groups that are displayed.
    breakdownItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the category fields.
    categorySort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaterfallChartSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'breakdownItemsLimit', 'waterfallChartSortConfiguration_breakdownItemsLimit' - The limit on the number of bar groups that are displayed.
--
-- 'categorySort', 'waterfallChartSortConfiguration_categorySort' - The sort configuration of the category fields.
newWaterfallChartSortConfiguration ::
  WaterfallChartSortConfiguration
newWaterfallChartSortConfiguration =
  WaterfallChartSortConfiguration'
    { breakdownItemsLimit =
        Prelude.Nothing,
      categorySort = Prelude.Nothing
    }

-- | The limit on the number of bar groups that are displayed.
waterfallChartSortConfiguration_breakdownItemsLimit :: Lens.Lens' WaterfallChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
waterfallChartSortConfiguration_breakdownItemsLimit = Lens.lens (\WaterfallChartSortConfiguration' {breakdownItemsLimit} -> breakdownItemsLimit) (\s@WaterfallChartSortConfiguration' {} a -> s {breakdownItemsLimit = a} :: WaterfallChartSortConfiguration)

-- | The sort configuration of the category fields.
waterfallChartSortConfiguration_categorySort :: Lens.Lens' WaterfallChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
waterfallChartSortConfiguration_categorySort = Lens.lens (\WaterfallChartSortConfiguration' {categorySort} -> categorySort) (\s@WaterfallChartSortConfiguration' {} a -> s {categorySort = a} :: WaterfallChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    WaterfallChartSortConfiguration
  where
  parseJSON =
    Data.withObject
      "WaterfallChartSortConfiguration"
      ( \x ->
          WaterfallChartSortConfiguration'
            Prelude.<$> (x Data..:? "BreakdownItemsLimit")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    WaterfallChartSortConfiguration
  where
  hashWithSalt
    _salt
    WaterfallChartSortConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` breakdownItemsLimit
        `Prelude.hashWithSalt` categorySort

instance
  Prelude.NFData
    WaterfallChartSortConfiguration
  where
  rnf WaterfallChartSortConfiguration' {..} =
    Prelude.rnf breakdownItemsLimit `Prelude.seq`
      Prelude.rnf categorySort

instance Data.ToJSON WaterfallChartSortConfiguration where
  toJSON WaterfallChartSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BreakdownItemsLimit" Data..=)
              Prelude.<$> breakdownItemsLimit,
            ("CategorySort" Data..=) Prelude.<$> categorySort
          ]
      )
