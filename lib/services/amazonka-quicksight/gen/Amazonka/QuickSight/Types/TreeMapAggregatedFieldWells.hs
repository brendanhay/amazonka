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
-- Module      : Amazonka.QuickSight.Types.TreeMapAggregatedFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TreeMapAggregatedFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.MeasureField

-- | Aggregated field wells of a tree map.
--
-- /See:/ 'newTreeMapAggregatedFieldWells' smart constructor.
data TreeMapAggregatedFieldWells = TreeMapAggregatedFieldWells'
  { -- | The color field well of a tree map. Values are grouped by aggregations
    -- based on group by fields.
    colors :: Prelude.Maybe [MeasureField],
    -- | The group by field well of a tree map. Values are grouped based on group
    -- by fields.
    groups :: Prelude.Maybe [DimensionField],
    -- | The size field well of a tree map. Values are aggregated based on group
    -- by fields.
    sizes :: Prelude.Maybe [MeasureField]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TreeMapAggregatedFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colors', 'treeMapAggregatedFieldWells_colors' - The color field well of a tree map. Values are grouped by aggregations
-- based on group by fields.
--
-- 'groups', 'treeMapAggregatedFieldWells_groups' - The group by field well of a tree map. Values are grouped based on group
-- by fields.
--
-- 'sizes', 'treeMapAggregatedFieldWells_sizes' - The size field well of a tree map. Values are aggregated based on group
-- by fields.
newTreeMapAggregatedFieldWells ::
  TreeMapAggregatedFieldWells
newTreeMapAggregatedFieldWells =
  TreeMapAggregatedFieldWells'
    { colors =
        Prelude.Nothing,
      groups = Prelude.Nothing,
      sizes = Prelude.Nothing
    }

-- | The color field well of a tree map. Values are grouped by aggregations
-- based on group by fields.
treeMapAggregatedFieldWells_colors :: Lens.Lens' TreeMapAggregatedFieldWells (Prelude.Maybe [MeasureField])
treeMapAggregatedFieldWells_colors = Lens.lens (\TreeMapAggregatedFieldWells' {colors} -> colors) (\s@TreeMapAggregatedFieldWells' {} a -> s {colors = a} :: TreeMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The group by field well of a tree map. Values are grouped based on group
-- by fields.
treeMapAggregatedFieldWells_groups :: Lens.Lens' TreeMapAggregatedFieldWells (Prelude.Maybe [DimensionField])
treeMapAggregatedFieldWells_groups = Lens.lens (\TreeMapAggregatedFieldWells' {groups} -> groups) (\s@TreeMapAggregatedFieldWells' {} a -> s {groups = a} :: TreeMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

-- | The size field well of a tree map. Values are aggregated based on group
-- by fields.
treeMapAggregatedFieldWells_sizes :: Lens.Lens' TreeMapAggregatedFieldWells (Prelude.Maybe [MeasureField])
treeMapAggregatedFieldWells_sizes = Lens.lens (\TreeMapAggregatedFieldWells' {sizes} -> sizes) (\s@TreeMapAggregatedFieldWells' {} a -> s {sizes = a} :: TreeMapAggregatedFieldWells) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TreeMapAggregatedFieldWells where
  parseJSON =
    Data.withObject
      "TreeMapAggregatedFieldWells"
      ( \x ->
          TreeMapAggregatedFieldWells'
            Prelude.<$> (x Data..:? "Colors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Groups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Sizes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TreeMapAggregatedFieldWells where
  hashWithSalt _salt TreeMapAggregatedFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` colors
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` sizes

instance Prelude.NFData TreeMapAggregatedFieldWells where
  rnf TreeMapAggregatedFieldWells' {..} =
    Prelude.rnf colors `Prelude.seq`
      Prelude.rnf groups `Prelude.seq`
        Prelude.rnf sizes

instance Data.ToJSON TreeMapAggregatedFieldWells where
  toJSON TreeMapAggregatedFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Colors" Data..=) Prelude.<$> colors,
            ("Groups" Data..=) Prelude.<$> groups,
            ("Sizes" Data..=) Prelude.<$> sizes
          ]
      )
