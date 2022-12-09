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
-- Module      : Amazonka.QuickSight.Types.BarChartSortConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BarChartSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | sort-configuration-description
--
-- /See:/ 'newBarChartSortConfiguration' smart constructor.
data BarChartSortConfiguration = BarChartSortConfiguration'
  { -- | The limit on the number of categories displayed in a bar chart.
    categoryItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of category fields.
    categorySort :: Prelude.Maybe [FieldSortOptions],
    -- | The limit on the number of values displayed in a bar chart.
    colorItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of color fields in a bar chart.
    colorSort :: Prelude.Maybe [FieldSortOptions],
    -- | The limit on the number of small multiples panels that are displayed.
    smallMultiplesLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the small multiples field.
    smallMultiplesSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BarChartSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryItemsLimit', 'barChartSortConfiguration_categoryItemsLimit' - The limit on the number of categories displayed in a bar chart.
--
-- 'categorySort', 'barChartSortConfiguration_categorySort' - The sort configuration of category fields.
--
-- 'colorItemsLimit', 'barChartSortConfiguration_colorItemsLimit' - The limit on the number of values displayed in a bar chart.
--
-- 'colorSort', 'barChartSortConfiguration_colorSort' - The sort configuration of color fields in a bar chart.
--
-- 'smallMultiplesLimitConfiguration', 'barChartSortConfiguration_smallMultiplesLimitConfiguration' - The limit on the number of small multiples panels that are displayed.
--
-- 'smallMultiplesSort', 'barChartSortConfiguration_smallMultiplesSort' - The sort configuration of the small multiples field.
newBarChartSortConfiguration ::
  BarChartSortConfiguration
newBarChartSortConfiguration =
  BarChartSortConfiguration'
    { categoryItemsLimit =
        Prelude.Nothing,
      categorySort = Prelude.Nothing,
      colorItemsLimit = Prelude.Nothing,
      colorSort = Prelude.Nothing,
      smallMultiplesLimitConfiguration =
        Prelude.Nothing,
      smallMultiplesSort = Prelude.Nothing
    }

-- | The limit on the number of categories displayed in a bar chart.
barChartSortConfiguration_categoryItemsLimit :: Lens.Lens' BarChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
barChartSortConfiguration_categoryItemsLimit = Lens.lens (\BarChartSortConfiguration' {categoryItemsLimit} -> categoryItemsLimit) (\s@BarChartSortConfiguration' {} a -> s {categoryItemsLimit = a} :: BarChartSortConfiguration)

-- | The sort configuration of category fields.
barChartSortConfiguration_categorySort :: Lens.Lens' BarChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
barChartSortConfiguration_categorySort = Lens.lens (\BarChartSortConfiguration' {categorySort} -> categorySort) (\s@BarChartSortConfiguration' {} a -> s {categorySort = a} :: BarChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The limit on the number of values displayed in a bar chart.
barChartSortConfiguration_colorItemsLimit :: Lens.Lens' BarChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
barChartSortConfiguration_colorItemsLimit = Lens.lens (\BarChartSortConfiguration' {colorItemsLimit} -> colorItemsLimit) (\s@BarChartSortConfiguration' {} a -> s {colorItemsLimit = a} :: BarChartSortConfiguration)

-- | The sort configuration of color fields in a bar chart.
barChartSortConfiguration_colorSort :: Lens.Lens' BarChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
barChartSortConfiguration_colorSort = Lens.lens (\BarChartSortConfiguration' {colorSort} -> colorSort) (\s@BarChartSortConfiguration' {} a -> s {colorSort = a} :: BarChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The limit on the number of small multiples panels that are displayed.
barChartSortConfiguration_smallMultiplesLimitConfiguration :: Lens.Lens' BarChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
barChartSortConfiguration_smallMultiplesLimitConfiguration = Lens.lens (\BarChartSortConfiguration' {smallMultiplesLimitConfiguration} -> smallMultiplesLimitConfiguration) (\s@BarChartSortConfiguration' {} a -> s {smallMultiplesLimitConfiguration = a} :: BarChartSortConfiguration)

-- | The sort configuration of the small multiples field.
barChartSortConfiguration_smallMultiplesSort :: Lens.Lens' BarChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
barChartSortConfiguration_smallMultiplesSort = Lens.lens (\BarChartSortConfiguration' {smallMultiplesSort} -> smallMultiplesSort) (\s@BarChartSortConfiguration' {} a -> s {smallMultiplesSort = a} :: BarChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BarChartSortConfiguration where
  parseJSON =
    Data.withObject
      "BarChartSortConfiguration"
      ( \x ->
          BarChartSortConfiguration'
            Prelude.<$> (x Data..:? "CategoryItemsLimit")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ColorItemsLimit")
            Prelude.<*> (x Data..:? "ColorSort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SmallMultiplesLimitConfiguration")
            Prelude.<*> ( x Data..:? "SmallMultiplesSort"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BarChartSortConfiguration where
  hashWithSalt _salt BarChartSortConfiguration' {..} =
    _salt `Prelude.hashWithSalt` categoryItemsLimit
      `Prelude.hashWithSalt` categorySort
      `Prelude.hashWithSalt` colorItemsLimit
      `Prelude.hashWithSalt` colorSort
      `Prelude.hashWithSalt` smallMultiplesLimitConfiguration
      `Prelude.hashWithSalt` smallMultiplesSort

instance Prelude.NFData BarChartSortConfiguration where
  rnf BarChartSortConfiguration' {..} =
    Prelude.rnf categoryItemsLimit
      `Prelude.seq` Prelude.rnf categorySort
      `Prelude.seq` Prelude.rnf colorItemsLimit
      `Prelude.seq` Prelude.rnf colorSort
      `Prelude.seq` Prelude.rnf smallMultiplesLimitConfiguration
      `Prelude.seq` Prelude.rnf smallMultiplesSort

instance Data.ToJSON BarChartSortConfiguration where
  toJSON BarChartSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryItemsLimit" Data..=)
              Prelude.<$> categoryItemsLimit,
            ("CategorySort" Data..=) Prelude.<$> categorySort,
            ("ColorItemsLimit" Data..=)
              Prelude.<$> colorItemsLimit,
            ("ColorSort" Data..=) Prelude.<$> colorSort,
            ("SmallMultiplesLimitConfiguration" Data..=)
              Prelude.<$> smallMultiplesLimitConfiguration,
            ("SmallMultiplesSort" Data..=)
              Prelude.<$> smallMultiplesSort
          ]
      )
