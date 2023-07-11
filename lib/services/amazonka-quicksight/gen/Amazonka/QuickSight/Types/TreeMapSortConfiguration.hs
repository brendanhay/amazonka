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
-- Module      : Amazonka.QuickSight.Types.TreeMapSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TreeMapSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FieldSortOptions
import Amazonka.QuickSight.Types.ItemsLimitConfiguration

-- | The sort configuration of a tree map.
--
-- /See:/ 'newTreeMapSortConfiguration' smart constructor.
data TreeMapSortConfiguration = TreeMapSortConfiguration'
  { -- | The limit on the number of groups that are displayed.
    treeMapGroupItemsLimitConfiguration :: Prelude.Maybe ItemsLimitConfiguration,
    -- | The sort configuration of group by fields.
    treeMapSort :: Prelude.Maybe [FieldSortOptions]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TreeMapSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'treeMapGroupItemsLimitConfiguration', 'treeMapSortConfiguration_treeMapGroupItemsLimitConfiguration' - The limit on the number of groups that are displayed.
--
-- 'treeMapSort', 'treeMapSortConfiguration_treeMapSort' - The sort configuration of group by fields.
newTreeMapSortConfiguration ::
  TreeMapSortConfiguration
newTreeMapSortConfiguration =
  TreeMapSortConfiguration'
    { treeMapGroupItemsLimitConfiguration =
        Prelude.Nothing,
      treeMapSort = Prelude.Nothing
    }

-- | The limit on the number of groups that are displayed.
treeMapSortConfiguration_treeMapGroupItemsLimitConfiguration :: Lens.Lens' TreeMapSortConfiguration (Prelude.Maybe ItemsLimitConfiguration)
treeMapSortConfiguration_treeMapGroupItemsLimitConfiguration = Lens.lens (\TreeMapSortConfiguration' {treeMapGroupItemsLimitConfiguration} -> treeMapGroupItemsLimitConfiguration) (\s@TreeMapSortConfiguration' {} a -> s {treeMapGroupItemsLimitConfiguration = a} :: TreeMapSortConfiguration)

-- | The sort configuration of group by fields.
treeMapSortConfiguration_treeMapSort :: Lens.Lens' TreeMapSortConfiguration (Prelude.Maybe [FieldSortOptions])
treeMapSortConfiguration_treeMapSort = Lens.lens (\TreeMapSortConfiguration' {treeMapSort} -> treeMapSort) (\s@TreeMapSortConfiguration' {} a -> s {treeMapSort = a} :: TreeMapSortConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TreeMapSortConfiguration where
  parseJSON =
    Data.withObject
      "TreeMapSortConfiguration"
      ( \x ->
          TreeMapSortConfiguration'
            Prelude.<$> (x Data..:? "TreeMapGroupItemsLimitConfiguration")
            Prelude.<*> (x Data..:? "TreeMapSort" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TreeMapSortConfiguration where
  hashWithSalt _salt TreeMapSortConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` treeMapGroupItemsLimitConfiguration
      `Prelude.hashWithSalt` treeMapSort

instance Prelude.NFData TreeMapSortConfiguration where
  rnf TreeMapSortConfiguration' {..} =
    Prelude.rnf treeMapGroupItemsLimitConfiguration
      `Prelude.seq` Prelude.rnf treeMapSort

instance Data.ToJSON TreeMapSortConfiguration where
  toJSON TreeMapSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TreeMapGroupItemsLimitConfiguration" Data..=)
              Prelude.<$> treeMapGroupItemsLimitConfiguration,
            ("TreeMapSort" Data..=) Prelude.<$> treeMapSort
          ]
      )
