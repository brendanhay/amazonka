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
-- Module      : Amazonka.QuickSight.Types.CategoryFilterConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.CategoryFilterConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CustomFilterConfiguration
import Amazonka.QuickSight.Types.CustomFilterListConfiguration
import Amazonka.QuickSight.Types.FilterListConfiguration

-- | The configuration for a @CategoryFilter@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newCategoryFilterConfiguration' smart constructor.
data CategoryFilterConfiguration = CategoryFilterConfiguration'
  { -- | A custom filter that filters based on a single value. This filter can be
    -- partially matched.
    customFilterConfiguration :: Prelude.Maybe CustomFilterConfiguration,
    -- | A list of custom filter values. In the Amazon QuickSight console, this
    -- filter type is called a custom filter list.
    customFilterListConfiguration :: Prelude.Maybe CustomFilterListConfiguration,
    -- | A list of filter configurations. In the Amazon QuickSight console, this
    -- filter type is called a filter list.
    filterListConfiguration :: Prelude.Maybe FilterListConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CategoryFilterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customFilterConfiguration', 'categoryFilterConfiguration_customFilterConfiguration' - A custom filter that filters based on a single value. This filter can be
-- partially matched.
--
-- 'customFilterListConfiguration', 'categoryFilterConfiguration_customFilterListConfiguration' - A list of custom filter values. In the Amazon QuickSight console, this
-- filter type is called a custom filter list.
--
-- 'filterListConfiguration', 'categoryFilterConfiguration_filterListConfiguration' - A list of filter configurations. In the Amazon QuickSight console, this
-- filter type is called a filter list.
newCategoryFilterConfiguration ::
  CategoryFilterConfiguration
newCategoryFilterConfiguration =
  CategoryFilterConfiguration'
    { customFilterConfiguration =
        Prelude.Nothing,
      customFilterListConfiguration =
        Prelude.Nothing,
      filterListConfiguration = Prelude.Nothing
    }

-- | A custom filter that filters based on a single value. This filter can be
-- partially matched.
categoryFilterConfiguration_customFilterConfiguration :: Lens.Lens' CategoryFilterConfiguration (Prelude.Maybe CustomFilterConfiguration)
categoryFilterConfiguration_customFilterConfiguration = Lens.lens (\CategoryFilterConfiguration' {customFilterConfiguration} -> customFilterConfiguration) (\s@CategoryFilterConfiguration' {} a -> s {customFilterConfiguration = a} :: CategoryFilterConfiguration)

-- | A list of custom filter values. In the Amazon QuickSight console, this
-- filter type is called a custom filter list.
categoryFilterConfiguration_customFilterListConfiguration :: Lens.Lens' CategoryFilterConfiguration (Prelude.Maybe CustomFilterListConfiguration)
categoryFilterConfiguration_customFilterListConfiguration = Lens.lens (\CategoryFilterConfiguration' {customFilterListConfiguration} -> customFilterListConfiguration) (\s@CategoryFilterConfiguration' {} a -> s {customFilterListConfiguration = a} :: CategoryFilterConfiguration)

-- | A list of filter configurations. In the Amazon QuickSight console, this
-- filter type is called a filter list.
categoryFilterConfiguration_filterListConfiguration :: Lens.Lens' CategoryFilterConfiguration (Prelude.Maybe FilterListConfiguration)
categoryFilterConfiguration_filterListConfiguration = Lens.lens (\CategoryFilterConfiguration' {filterListConfiguration} -> filterListConfiguration) (\s@CategoryFilterConfiguration' {} a -> s {filterListConfiguration = a} :: CategoryFilterConfiguration)

instance Data.FromJSON CategoryFilterConfiguration where
  parseJSON =
    Data.withObject
      "CategoryFilterConfiguration"
      ( \x ->
          CategoryFilterConfiguration'
            Prelude.<$> (x Data..:? "CustomFilterConfiguration")
            Prelude.<*> (x Data..:? "CustomFilterListConfiguration")
            Prelude.<*> (x Data..:? "FilterListConfiguration")
      )

instance Prelude.Hashable CategoryFilterConfiguration where
  hashWithSalt _salt CategoryFilterConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` customFilterConfiguration
      `Prelude.hashWithSalt` customFilterListConfiguration
      `Prelude.hashWithSalt` filterListConfiguration

instance Prelude.NFData CategoryFilterConfiguration where
  rnf CategoryFilterConfiguration' {..} =
    Prelude.rnf customFilterConfiguration
      `Prelude.seq` Prelude.rnf customFilterListConfiguration
      `Prelude.seq` Prelude.rnf filterListConfiguration

instance Data.ToJSON CategoryFilterConfiguration where
  toJSON CategoryFilterConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomFilterConfiguration" Data..=)
              Prelude.<$> customFilterConfiguration,
            ("CustomFilterListConfiguration" Data..=)
              Prelude.<$> customFilterListConfiguration,
            ("FilterListConfiguration" Data..=)
              Prelude.<$> filterListConfiguration
          ]
      )
