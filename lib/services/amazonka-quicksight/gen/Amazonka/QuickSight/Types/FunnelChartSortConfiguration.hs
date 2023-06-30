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
-- Module      : Amazonka.QuickSight.Types.FunnelChartSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FunnelChartSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a @FunnelChartVisual@.
--
-- /See:/ 'newFunnelChartSortConfiguration' smart constructor.
data FunnelChartSortConfiguration = FunnelChartSortConfiguration'
  { -- | The limit on the number of categories displayed.
    categoryItemsLimit :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of the category fields.
    categorySort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunnelChartSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryItemsLimit', 'funnelChartSortConfiguration_categoryItemsLimit' - The limit on the number of categories displayed.
--
-- 'categorySort', 'funnelChartSortConfiguration_categorySort' - The sort configuration of the category fields.
newFunnelChartSortConfiguration ::
  FunnelChartSortConfiguration
newFunnelChartSortConfiguration =
  FunnelChartSortConfiguration'
    { categoryItemsLimit =
        Prelude.Nothing,
      categorySort = Prelude.Nothing
    }

-- | The limit on the number of categories displayed.
funnelChartSortConfiguration_categoryItemsLimit :: Lens.Lens' FunnelChartSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
funnelChartSortConfiguration_categoryItemsLimit = Lens.lens (\FunnelChartSortConfiguration' {categoryItemsLimit} -> categoryItemsLimit) (\s@FunnelChartSortConfiguration' {} a -> s {categoryItemsLimit = a} :: FunnelChartSortConfiguration)

-- | The sort configuration of the category fields.
funnelChartSortConfiguration_categorySort :: Lens.Lens' FunnelChartSortConfiguration (Prelude.Maybe [FieldSortOptions])
funnelChartSortConfiguration_categorySort = Lens.lens (\FunnelChartSortConfiguration' {categorySort} -> categorySort) (\s@FunnelChartSortConfiguration' {} a -> s {categorySort = a} :: FunnelChartSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FunnelChartSortConfiguration where
  parseJSON =
    Data.withObject
      "FunnelChartSortConfiguration"
      ( \x ->
          FunnelChartSortConfiguration'
            Prelude.<$> (x Data..:? "CategoryItemsLimit")
            Prelude.<*> (x Data..:? "CategorySort" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    FunnelChartSortConfiguration
  where
  hashWithSalt _salt FunnelChartSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryItemsLimit
      `Prelude.hashWithSalt` categorySort

instance Prelude.NFData FunnelChartSortConfiguration where
  rnf FunnelChartSortConfiguration' {..} =
    Prelude.rnf categoryItemsLimit
      `Prelude.seq` Prelude.rnf categorySort

instance Data.ToJSON FunnelChartSortConfiguration where
  toJSON FunnelChartSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryItemsLimit" Data..=)
              Prelude.<$> categoryItemsLimit,
            ("CategorySort" Data..=) Prelude.<$> categorySort
          ]
      )
