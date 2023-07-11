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
-- Module      : Amazonka.QuickSight.Types.HeatMapSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HeatMapSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a heat map.
--
-- /See:/ 'newHeatMapSortConfiguration' smart constructor.
data HeatMapSortConfiguration = HeatMapSortConfiguration'
  { -- | The limit on the number of columns that are displayed in a heat map.
    heatMapColumnItemsLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The column sort configuration for heat map for columns that aren\'t a
    -- part of a field well.
    heatMapColumnSort :: Prelude.Maybe [FieldSortOptions],
    -- | The limit on the number of rows that are displayed in a heat map.
    heatMapRowItemsLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The field sort configuration of the rows fields.
    heatMapRowSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeatMapSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'heatMapColumnItemsLimitConfiguration', 'heatMapSortConfiguration_heatMapColumnItemsLimitConfiguration' - The limit on the number of columns that are displayed in a heat map.
--
-- 'heatMapColumnSort', 'heatMapSortConfiguration_heatMapColumnSort' - The column sort configuration for heat map for columns that aren\'t a
-- part of a field well.
--
-- 'heatMapRowItemsLimitConfiguration', 'heatMapSortConfiguration_heatMapRowItemsLimitConfiguration' - The limit on the number of rows that are displayed in a heat map.
--
-- 'heatMapRowSort', 'heatMapSortConfiguration_heatMapRowSort' - The field sort configuration of the rows fields.
newHeatMapSortConfiguration ::
  HeatMapSortConfiguration
newHeatMapSortConfiguration =
  HeatMapSortConfiguration'
    { heatMapColumnItemsLimitConfiguration =
        Prelude.Nothing,
      heatMapColumnSort = Prelude.Nothing,
      heatMapRowItemsLimitConfiguration =
        Prelude.Nothing,
      heatMapRowSort = Prelude.Nothing
    }

-- | The limit on the number of columns that are displayed in a heat map.
heatMapSortConfiguration_heatMapColumnItemsLimitConfiguration :: Lens.Lens' HeatMapSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
heatMapSortConfiguration_heatMapColumnItemsLimitConfiguration = Lens.lens (\HeatMapSortConfiguration' {heatMapColumnItemsLimitConfiguration} -> heatMapColumnItemsLimitConfiguration) (\s@HeatMapSortConfiguration' {} a -> s {heatMapColumnItemsLimitConfiguration = a} :: HeatMapSortConfiguration)

-- | The column sort configuration for heat map for columns that aren\'t a
-- part of a field well.
heatMapSortConfiguration_heatMapColumnSort :: Lens.Lens' HeatMapSortConfiguration (Prelude.Maybe [FieldSortOptions])
heatMapSortConfiguration_heatMapColumnSort = Lens.lens (\HeatMapSortConfiguration' {heatMapColumnSort} -> heatMapColumnSort) (\s@HeatMapSortConfiguration' {} a -> s {heatMapColumnSort = a} :: HeatMapSortConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The limit on the number of rows that are displayed in a heat map.
heatMapSortConfiguration_heatMapRowItemsLimitConfiguration :: Lens.Lens' HeatMapSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
heatMapSortConfiguration_heatMapRowItemsLimitConfiguration = Lens.lens (\HeatMapSortConfiguration' {heatMapRowItemsLimitConfiguration} -> heatMapRowItemsLimitConfiguration) (\s@HeatMapSortConfiguration' {} a -> s {heatMapRowItemsLimitConfiguration = a} :: HeatMapSortConfiguration)

-- | The field sort configuration of the rows fields.
heatMapSortConfiguration_heatMapRowSort :: Lens.Lens' HeatMapSortConfiguration (Prelude.Maybe [FieldSortOptions])
heatMapSortConfiguration_heatMapRowSort = Lens.lens (\HeatMapSortConfiguration' {heatMapRowSort} -> heatMapRowSort) (\s@HeatMapSortConfiguration' {} a -> s {heatMapRowSort = a} :: HeatMapSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON HeatMapSortConfiguration where
  parseJSON =
    Data.withObject
      "HeatMapSortConfiguration"
      ( \x ->
          HeatMapSortConfiguration'
            Prelude.<$> (x Data..:? "HeatMapColumnItemsLimitConfiguration")
            Prelude.<*> ( x
                            Data..:? "HeatMapColumnSort"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "HeatMapRowItemsLimitConfiguration")
            Prelude.<*> ( x
                            Data..:? "HeatMapRowSort"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable HeatMapSortConfiguration where
  hashWithSalt _salt HeatMapSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` heatMapColumnItemsLimitConfiguration
      `Prelude.hashWithSalt` heatMapColumnSort
      `Prelude.hashWithSalt` heatMapRowItemsLimitConfiguration
      `Prelude.hashWithSalt` heatMapRowSort

instance Prelude.NFData HeatMapSortConfiguration where
  rnf HeatMapSortConfiguration' {..} =
    Prelude.rnf heatMapColumnItemsLimitConfiguration
      `Prelude.seq` Prelude.rnf heatMapColumnSort
      `Prelude.seq` Prelude.rnf heatMapRowItemsLimitConfiguration
      `Prelude.seq` Prelude.rnf heatMapRowSort

instance Data.ToJSON HeatMapSortConfiguration where
  toJSON HeatMapSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HeatMapColumnItemsLimitConfiguration" Data..=)
              Prelude.<$> heatMapColumnItemsLimitConfiguration,
            ("HeatMapColumnSort" Data..=)
              Prelude.<$> heatMapColumnSort,
            ("HeatMapRowItemsLimitConfiguration" Data..=)
              Prelude.<$> heatMapRowItemsLimitConfiguration,
            ("HeatMapRowSort" Data..=)
              Prelude.<$> heatMapRowSort
          ]
      )
