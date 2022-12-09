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
-- Module      : Amazonka.QuickSight.Types.LineChartSortConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a line chart.
--
-- /See:/ 'newLineChartSortConfiguration' smart constructor.
data LineChartSortConfiguration = LineChartSortConfiguration'
  { -- | The limit on the number of categories that are displayed in a line
    -- chart.
    categoryItemsLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the category fields.
    categorySort :: Prelude.Maybe [FieldSortOptions],
    -- | The limit on the number of lines that are displayed in a line chart.
    colorItemsLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The limit on the number of small multiples panels that are displayed.
    smallMultiplesLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the small multiples field.
    smallMultiplesSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryItemsLimitConfiguration', 'lineChartSortConfiguration_categoryItemsLimitConfiguration' - The limit on the number of categories that are displayed in a line
-- chart.
--
-- 'categorySort', 'lineChartSortConfiguration_categorySort' - The sort configuration of the category fields.
--
-- 'colorItemsLimitConfiguration', 'lineChartSortConfiguration_colorItemsLimitConfiguration' - The limit on the number of lines that are displayed in a line chart.
--
-- 'smallMultiplesLimitConfiguration', 'lineChartSortConfiguration_smallMultiplesLimitConfiguration' - The limit on the number of small multiples panels that are displayed.
--
-- 'smallMultiplesSort', 'lineChartSortConfiguration_smallMultiplesSort' - The sort configuration of the small multiples field.
newLineChartSortConfiguration ::
  LineChartSortConfiguration
newLineChartSortConfiguration =
  LineChartSortConfiguration'
    { categoryItemsLimitConfiguration =
        Prelude.Nothing,
      categorySort = Prelude.Nothing,
      colorItemsLimitConfiguration = Prelude.Nothing,
      smallMultiplesLimitConfiguration =
        Prelude.Nothing,
      smallMultiplesSort = Prelude.Nothing
    }

-- | The limit on the number of categories that are displayed in a line
-- chart.
lineChartSortConfiguration_categoryItemsLimitConfiguration :: Lens.Lens' LineChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
lineChartSortConfiguration_categoryItemsLimitConfiguration = Lens.lens (\LineChartSortConfiguration' {categoryItemsLimitConfiguration} -> categoryItemsLimitConfiguration) (\s@LineChartSortConfiguration' {} a -> s {categoryItemsLimitConfiguration = a} :: LineChartSortConfiguration)

-- | The sort configuration of the category fields.
lineChartSortConfiguration_categorySort :: Lens.Lens' LineChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
lineChartSortConfiguration_categorySort = Lens.lens (\LineChartSortConfiguration' {categorySort} -> categorySort) (\s@LineChartSortConfiguration' {} a -> s {categorySort = a} :: LineChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The limit on the number of lines that are displayed in a line chart.
lineChartSortConfiguration_colorItemsLimitConfiguration :: Lens.Lens' LineChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
lineChartSortConfiguration_colorItemsLimitConfiguration = Lens.lens (\LineChartSortConfiguration' {colorItemsLimitConfiguration} -> colorItemsLimitConfiguration) (\s@LineChartSortConfiguration' {} a -> s {colorItemsLimitConfiguration = a} :: LineChartSortConfiguration)

-- | The limit on the number of small multiples panels that are displayed.
lineChartSortConfiguration_smallMultiplesLimitConfiguration :: Lens.Lens' LineChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
lineChartSortConfiguration_smallMultiplesLimitConfiguration = Lens.lens (\LineChartSortConfiguration' {smallMultiplesLimitConfiguration} -> smallMultiplesLimitConfiguration) (\s@LineChartSortConfiguration' {} a -> s {smallMultiplesLimitConfiguration = a} :: LineChartSortConfiguration)

-- | The sort configuration of the small multiples field.
lineChartSortConfiguration_smallMultiplesSort :: Lens.Lens' LineChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
lineChartSortConfiguration_smallMultiplesSort = Lens.lens (\LineChartSortConfiguration' {smallMultiplesSort} -> smallMultiplesSort) (\s@LineChartSortConfiguration' {} a -> s {smallMultiplesSort = a} :: LineChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LineChartSortConfiguration where
  parseJSON =
    Data.withObject
      "LineChartSortConfiguration"
      ( \x ->
          LineChartSortConfiguration'
            Prelude.<$> (x Data..:? "CategoryItemsLimitConfiguration")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ColorItemsLimitConfiguration")
            Prelude.<*> (x Data..:? "SmallMultiplesLimitConfiguration")
            Prelude.<*> ( x Data..:? "SmallMultiplesSort"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LineChartSortConfiguration where
  hashWithSalt _salt LineChartSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryItemsLimitConfiguration
      `Prelude.hashWithSalt` categorySort
      `Prelude.hashWithSalt` colorItemsLimitConfiguration
      `Prelude.hashWithSalt` smallMultiplesLimitConfiguration
      `Prelude.hashWithSalt` smallMultiplesSort

instance Prelude.NFData LineChartSortConfiguration where
  rnf LineChartSortConfiguration' {..} =
    Prelude.rnf categoryItemsLimitConfiguration
      `Prelude.seq` Prelude.rnf categorySort
      `Prelude.seq` Prelude.rnf colorItemsLimitConfiguration
      `Prelude.seq` Prelude.rnf smallMultiplesLimitConfiguration
      `Prelude.seq` Prelude.rnf smallMultiplesSort

instance Data.ToJSON LineChartSortConfiguration where
  toJSON LineChartSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryItemsLimitConfiguration" Data..=)
              Prelude.<$> categoryItemsLimitConfiguration,
            ("CategorySort" Data..=) Prelude.<$> categorySort,
            ("ColorItemsLimitConfiguration" Data..=)
              Prelude.<$> colorItemsLimitConfiguration,
            ("SmallMultiplesLimitConfiguration" Data..=)
              Prelude.<$> smallMultiplesLimitConfiguration,
            ("SmallMultiplesSort" Data..=)
              Prelude.<$> smallMultiplesSort
          ]
      )
