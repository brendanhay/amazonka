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
-- Module      : Amazonka.QuickSight.Types.TreeMapFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TreeMapFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TreeMapAggregatedFieldWells

-- | The field wells of a tree map.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newTreeMapFieldWells' smart constructor.
data TreeMapFieldWells = TreeMapFieldWells'
  { -- | The aggregated field wells of a tree map.
    treeMapAggregatedFieldWells :: Prelude.Maybe TreeMapAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TreeMapFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'treeMapAggregatedFieldWells', 'treeMapFieldWells_treeMapAggregatedFieldWells' - The aggregated field wells of a tree map.
newTreeMapFieldWells ::
  TreeMapFieldWells
newTreeMapFieldWells =
  TreeMapFieldWells'
    { treeMapAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field wells of a tree map.
treeMapFieldWells_treeMapAggregatedFieldWells :: Lens.Lens' TreeMapFieldWells (Prelude.Maybe TreeMapAggregatedFieldWells)
treeMapFieldWells_treeMapAggregatedFieldWells = Lens.lens (\TreeMapFieldWells' {treeMapAggregatedFieldWells} -> treeMapAggregatedFieldWells) (\s@TreeMapFieldWells' {} a -> s {treeMapAggregatedFieldWells = a} :: TreeMapFieldWells)

instance Data.FromJSON TreeMapFieldWells where
  parseJSON =
    Data.withObject
      "TreeMapFieldWells"
      ( \x ->
          TreeMapFieldWells'
            Prelude.<$> (x Data..:? "TreeMapAggregatedFieldWells")
      )

instance Prelude.Hashable TreeMapFieldWells where
  hashWithSalt _salt TreeMapFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` treeMapAggregatedFieldWells

instance Prelude.NFData TreeMapFieldWells where
  rnf TreeMapFieldWells' {..} =
    Prelude.rnf treeMapAggregatedFieldWells

instance Data.ToJSON TreeMapFieldWells where
  toJSON TreeMapFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TreeMapAggregatedFieldWells" Data..=)
              Prelude.<$> treeMapAggregatedFieldWells
          ]
      )
