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
-- Module      : Amazonka.QuickSight.Types.PieChartSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PieChartSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a pie chart.
--
-- /See:/ 'newPieChartSortConfiguration' smart constructor.
data PieChartSortConfiguration = PieChartSortConfiguration'
  { -- | The limit on the number of categories that are displayed in a pie chart.
    categoryItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the category fields.
    categorySort :: Prelude.Maybe [FieldSortOptions],
    -- | The limit on the number of small multiples panels that are displayed.
    smallMultiplesLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the small multiples field.
    smallMultiplesSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PieChartSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryItemsLimit', 'pieChartSortConfiguration_categoryItemsLimit' - The limit on the number of categories that are displayed in a pie chart.
--
-- 'categorySort', 'pieChartSortConfiguration_categorySort' - The sort configuration of the category fields.
--
-- 'smallMultiplesLimitConfiguration', 'pieChartSortConfiguration_smallMultiplesLimitConfiguration' - The limit on the number of small multiples panels that are displayed.
--
-- 'smallMultiplesSort', 'pieChartSortConfiguration_smallMultiplesSort' - The sort configuration of the small multiples field.
newPieChartSortConfiguration ::
  PieChartSortConfiguration
newPieChartSortConfiguration =
  PieChartSortConfiguration'
    { categoryItemsLimit =
        Prelude.Nothing,
      categorySort = Prelude.Nothing,
      smallMultiplesLimitConfiguration =
        Prelude.Nothing,
      smallMultiplesSort = Prelude.Nothing
    }

-- | The limit on the number of categories that are displayed in a pie chart.
pieChartSortConfiguration_categoryItemsLimit :: Lens.Lens' PieChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
pieChartSortConfiguration_categoryItemsLimit = Lens.lens (\PieChartSortConfiguration' {categoryItemsLimit} -> categoryItemsLimit) (\s@PieChartSortConfiguration' {} a -> s {categoryItemsLimit = a} :: PieChartSortConfiguration)

-- | The sort configuration of the category fields.
pieChartSortConfiguration_categorySort :: Lens.Lens' PieChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
pieChartSortConfiguration_categorySort = Lens.lens (\PieChartSortConfiguration' {categorySort} -> categorySort) (\s@PieChartSortConfiguration' {} a -> s {categorySort = a} :: PieChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The limit on the number of small multiples panels that are displayed.
pieChartSortConfiguration_smallMultiplesLimitConfiguration :: Lens.Lens' PieChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
pieChartSortConfiguration_smallMultiplesLimitConfiguration = Lens.lens (\PieChartSortConfiguration' {smallMultiplesLimitConfiguration} -> smallMultiplesLimitConfiguration) (\s@PieChartSortConfiguration' {} a -> s {smallMultiplesLimitConfiguration = a} :: PieChartSortConfiguration)

-- | The sort configuration of the small multiples field.
pieChartSortConfiguration_smallMultiplesSort :: Lens.Lens' PieChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
pieChartSortConfiguration_smallMultiplesSort = Lens.lens (\PieChartSortConfiguration' {smallMultiplesSort} -> smallMultiplesSort) (\s@PieChartSortConfiguration' {} a -> s {smallMultiplesSort = a} :: PieChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PieChartSortConfiguration where
  parseJSON =
    Data.withObject
      "PieChartSortConfiguration"
      ( \x ->
          PieChartSortConfiguration'
            Prelude.<$> (x Data..:? "CategoryItemsLimit")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SmallMultiplesLimitConfiguration")
            Prelude.<*> ( x Data..:? "SmallMultiplesSort"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PieChartSortConfiguration where
  hashWithSalt _salt PieChartSortConfiguration' {..} =
    _salt `Prelude.hashWithSalt` categoryItemsLimit
      `Prelude.hashWithSalt` categorySort
      `Prelude.hashWithSalt` smallMultiplesLimitConfiguration
      `Prelude.hashWithSalt` smallMultiplesSort

instance Prelude.NFData PieChartSortConfiguration where
  rnf PieChartSortConfiguration' {..} =
    Prelude.rnf categoryItemsLimit
      `Prelude.seq` Prelude.rnf categorySort
      `Prelude.seq` Prelude.rnf smallMultiplesLimitConfiguration
      `Prelude.seq` Prelude.rnf smallMultiplesSort

instance Data.ToJSON PieChartSortConfiguration where
  toJSON PieChartSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryItemsLimit" Data..=)
              Prelude.<$> categoryItemsLimit,
            ("CategorySort" Data..=) Prelude.<$> categorySort,
            ("SmallMultiplesLimitConfiguration" Data..=)
              Prelude.<$> smallMultiplesLimitConfiguration,
            ("SmallMultiplesSort" Data..=)
              Prelude.<$> smallMultiplesSort
          ]
      )
