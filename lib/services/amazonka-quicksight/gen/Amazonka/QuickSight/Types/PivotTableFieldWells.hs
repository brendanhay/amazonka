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
-- Module      : Amazonka.QuickSight.Types.PivotTableFieldWells
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableAggregatedFieldWells

-- | The field wells for a pivot table visual.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newPivotTableFieldWells' smart constructor.
data PivotTableFieldWells = PivotTableFieldWells'
  { -- | The aggregated field well for the pivot table.
    pivotTableAggregatedFieldWells :: Prelude.Maybe PivotTableAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pivotTableAggregatedFieldWells', 'pivotTableFieldWells_pivotTableAggregatedFieldWells' - The aggregated field well for the pivot table.
newPivotTableFieldWells ::
  PivotTableFieldWells
newPivotTableFieldWells =
  PivotTableFieldWells'
    { pivotTableAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field well for the pivot table.
pivotTableFieldWells_pivotTableAggregatedFieldWells :: Lens.Lens' PivotTableFieldWells (Prelude.Maybe PivotTableAggregatedFieldWells)
pivotTableFieldWells_pivotTableAggregatedFieldWells = Lens.lens (\PivotTableFieldWells' {pivotTableAggregatedFieldWells} -> pivotTableAggregatedFieldWells) (\s@PivotTableFieldWells' {} a -> s {pivotTableAggregatedFieldWells = a} :: PivotTableFieldWells)

instance Data.FromJSON PivotTableFieldWells where
  parseJSON =
    Data.withObject
      "PivotTableFieldWells"
      ( \x ->
          PivotTableFieldWells'
            Prelude.<$> (x Data..:? "PivotTableAggregatedFieldWells")
      )

instance Prelude.Hashable PivotTableFieldWells where
  hashWithSalt _salt PivotTableFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` pivotTableAggregatedFieldWells

instance Prelude.NFData PivotTableFieldWells where
  rnf PivotTableFieldWells' {..} =
    Prelude.rnf pivotTableAggregatedFieldWells

instance Data.ToJSON PivotTableFieldWells where
  toJSON PivotTableFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PivotTableAggregatedFieldWells" Data..=)
              Prelude.<$> pivotTableAggregatedFieldWells
          ]
      )
