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
-- Module      : Amazonka.QuickSight.Types.RadarChartSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RadarChartSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a @RadarChartVisual@.
--
-- /See:/ 'newRadarChartSortConfiguration' smart constructor.
data RadarChartSortConfiguration = RadarChartSortConfiguration'
  { -- | The category items limit for a radar chart.
    categoryItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The category sort options of a radar chart.
    categorySort :: Prelude.Maybe [FieldSortOptions],
    -- | The color items limit of a radar chart.
    colorItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The color sort configuration of a radar chart.
    colorSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadarChartSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryItemsLimit', 'radarChartSortConfiguration_categoryItemsLimit' - The category items limit for a radar chart.
--
-- 'categorySort', 'radarChartSortConfiguration_categorySort' - The category sort options of a radar chart.
--
-- 'colorItemsLimit', 'radarChartSortConfiguration_colorItemsLimit' - The color items limit of a radar chart.
--
-- 'colorSort', 'radarChartSortConfiguration_colorSort' - The color sort configuration of a radar chart.
newRadarChartSortConfiguration ::
  RadarChartSortConfiguration
newRadarChartSortConfiguration =
  RadarChartSortConfiguration'
    { categoryItemsLimit =
        Prelude.Nothing,
      categorySort = Prelude.Nothing,
      colorItemsLimit = Prelude.Nothing,
      colorSort = Prelude.Nothing
    }

-- | The category items limit for a radar chart.
radarChartSortConfiguration_categoryItemsLimit :: Lens.Lens' RadarChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
radarChartSortConfiguration_categoryItemsLimit = Lens.lens (\RadarChartSortConfiguration' {categoryItemsLimit} -> categoryItemsLimit) (\s@RadarChartSortConfiguration' {} a -> s {categoryItemsLimit = a} :: RadarChartSortConfiguration)

-- | The category sort options of a radar chart.
radarChartSortConfiguration_categorySort :: Lens.Lens' RadarChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
radarChartSortConfiguration_categorySort = Lens.lens (\RadarChartSortConfiguration' {categorySort} -> categorySort) (\s@RadarChartSortConfiguration' {} a -> s {categorySort = a} :: RadarChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The color items limit of a radar chart.
radarChartSortConfiguration_colorItemsLimit :: Lens.Lens' RadarChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
radarChartSortConfiguration_colorItemsLimit = Lens.lens (\RadarChartSortConfiguration' {colorItemsLimit} -> colorItemsLimit) (\s@RadarChartSortConfiguration' {} a -> s {colorItemsLimit = a} :: RadarChartSortConfiguration)

-- | The color sort configuration of a radar chart.
radarChartSortConfiguration_colorSort :: Lens.Lens' RadarChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
radarChartSortConfiguration_colorSort = Lens.lens (\RadarChartSortConfiguration' {colorSort} -> colorSort) (\s@RadarChartSortConfiguration' {} a -> s {colorSort = a} :: RadarChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RadarChartSortConfiguration where
  parseJSON =
    Data.withObject
      "RadarChartSortConfiguration"
      ( \x ->
          RadarChartSortConfiguration'
            Prelude.<$> (x Data..:? "CategoryItemsLimit")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ColorItemsLimit")
            Prelude.<*> (x Data..:? "ColorSort" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RadarChartSortConfiguration where
  hashWithSalt _salt RadarChartSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryItemsLimit
      `Prelude.hashWithSalt` categorySort
      `Prelude.hashWithSalt` colorItemsLimit
      `Prelude.hashWithSalt` colorSort

instance Prelude.NFData RadarChartSortConfiguration where
  rnf RadarChartSortConfiguration' {..} =
    Prelude.rnf categoryItemsLimit
      `Prelude.seq` Prelude.rnf categorySort
      `Prelude.seq` Prelude.rnf colorItemsLimit
      `Prelude.seq` Prelude.rnf colorSort

instance Data.ToJSON RadarChartSortConfiguration where
  toJSON RadarChartSortConfiguration' {..} =
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
