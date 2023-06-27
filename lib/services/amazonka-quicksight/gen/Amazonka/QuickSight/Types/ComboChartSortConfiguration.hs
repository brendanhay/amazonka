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
-- Module      : Amazonka.QuickSight.Types.ComboChartSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComboChartSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a @ComboChartVisual@.
--
-- /See:/ 'newComboChartSortConfiguration' smart constructor.
data ComboChartSortConfiguration = ComboChartSortConfiguration'
  { -- | The item limit configuration for the category field well of a combo
    -- chart.
    categoryItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the category field well in a combo chart.
    categorySort :: Prelude.Maybe [FieldSortOptions],
    -- | The item limit configuration of the color field well in a combo chart.
    colorItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the color field well in a combo chart.
    colorSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComboChartSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryItemsLimit', 'comboChartSortConfiguration_categoryItemsLimit' - The item limit configuration for the category field well of a combo
-- chart.
--
-- 'categorySort', 'comboChartSortConfiguration_categorySort' - The sort configuration of the category field well in a combo chart.
--
-- 'colorItemsLimit', 'comboChartSortConfiguration_colorItemsLimit' - The item limit configuration of the color field well in a combo chart.
--
-- 'colorSort', 'comboChartSortConfiguration_colorSort' - The sort configuration of the color field well in a combo chart.
newComboChartSortConfiguration ::
  ComboChartSortConfiguration
newComboChartSortConfiguration =
  ComboChartSortConfiguration'
    { categoryItemsLimit =
        Prelude.Nothing,
      categorySort = Prelude.Nothing,
      colorItemsLimit = Prelude.Nothing,
      colorSort = Prelude.Nothing
    }

-- | The item limit configuration for the category field well of a combo
-- chart.
comboChartSortConfiguration_categoryItemsLimit :: Lens.Lens' ComboChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
comboChartSortConfiguration_categoryItemsLimit = Lens.lens (\ComboChartSortConfiguration' {categoryItemsLimit} -> categoryItemsLimit) (\s@ComboChartSortConfiguration' {} a -> s {categoryItemsLimit = a} :: ComboChartSortConfiguration)

-- | The sort configuration of the category field well in a combo chart.
comboChartSortConfiguration_categorySort :: Lens.Lens' ComboChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
comboChartSortConfiguration_categorySort = Lens.lens (\ComboChartSortConfiguration' {categorySort} -> categorySort) (\s@ComboChartSortConfiguration' {} a -> s {categorySort = a} :: ComboChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The item limit configuration of the color field well in a combo chart.
comboChartSortConfiguration_colorItemsLimit :: Lens.Lens' ComboChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
comboChartSortConfiguration_colorItemsLimit = Lens.lens (\ComboChartSortConfiguration' {colorItemsLimit} -> colorItemsLimit) (\s@ComboChartSortConfiguration' {} a -> s {colorItemsLimit = a} :: ComboChartSortConfiguration)

-- | The sort configuration of the color field well in a combo chart.
comboChartSortConfiguration_colorSort :: Lens.Lens' ComboChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
comboChartSortConfiguration_colorSort = Lens.lens (\ComboChartSortConfiguration' {colorSort} -> colorSort) (\s@ComboChartSortConfiguration' {} a -> s {colorSort = a} :: ComboChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ComboChartSortConfiguration where
  parseJSON =
    Data.withObject
      "ComboChartSortConfiguration"
      ( \x ->
          ComboChartSortConfiguration'
            Prelude.<$> (x Data..:? "CategoryItemsLimit")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ColorItemsLimit")
            Prelude.<*> (x Data..:? "ColorSort" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ComboChartSortConfiguration where
  hashWithSalt _salt ComboChartSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryItemsLimit
      `Prelude.hashWithSalt` categorySort
      `Prelude.hashWithSalt` colorItemsLimit
      `Prelude.hashWithSalt` colorSort

instance Prelude.NFData ComboChartSortConfiguration where
  rnf ComboChartSortConfiguration' {..} =
    Prelude.rnf categoryItemsLimit
      `Prelude.seq` Prelude.rnf categorySort
      `Prelude.seq` Prelude.rnf colorItemsLimit
      `Prelude.seq` Prelude.rnf colorSort

instance Data.ToJSON ComboChartSortConfiguration where
  toJSON ComboChartSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryItemsLimit" Data..=)
              Prelude.<$> categoryItemsLimit,
            ("CategorySort" Data..=) Prelude.<$> categorySort,
            ("ColorItemsLimit" Data..=)
              Prelude.<$> colorItemsLimit,
            ("ColorSort" Data..=) Prelude.<$> colorSort
          ]
      )
