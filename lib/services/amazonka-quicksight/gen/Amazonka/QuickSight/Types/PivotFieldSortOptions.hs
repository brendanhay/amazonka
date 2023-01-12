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
-- Module      : Amazonka.QuickSight.Types.PivotFieldSortOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotFieldSortOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableSortBy

-- | The field sort options for a pivot table sort configuration.
--
-- /See:/ 'newPivotFieldSortOptions' smart constructor.
data PivotFieldSortOptions = PivotFieldSortOptions'
  { -- | The field ID for the field sort options.
    fieldId :: Prelude.Text,
    -- | The sort by field for the field sort options.
    sortBy :: PivotTableSortBy
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotFieldSortOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldId', 'pivotFieldSortOptions_fieldId' - The field ID for the field sort options.
--
-- 'sortBy', 'pivotFieldSortOptions_sortBy' - The sort by field for the field sort options.
newPivotFieldSortOptions ::
  -- | 'fieldId'
  Prelude.Text ->
  -- | 'sortBy'
  PivotTableSortBy ->
  PivotFieldSortOptions
newPivotFieldSortOptions pFieldId_ pSortBy_ =
  PivotFieldSortOptions'
    { fieldId = pFieldId_,
      sortBy = pSortBy_
    }

-- | The field ID for the field sort options.
pivotFieldSortOptions_fieldId :: Lens.Lens' PivotFieldSortOptions Prelude.Text
pivotFieldSortOptions_fieldId = Lens.lens (\PivotFieldSortOptions' {fieldId} -> fieldId) (\s@PivotFieldSortOptions' {} a -> s {fieldId = a} :: PivotFieldSortOptions)

-- | The sort by field for the field sort options.
pivotFieldSortOptions_sortBy :: Lens.Lens' PivotFieldSortOptions PivotTableSortBy
pivotFieldSortOptions_sortBy = Lens.lens (\PivotFieldSortOptions' {sortBy} -> sortBy) (\s@PivotFieldSortOptions' {} a -> s {sortBy = a} :: PivotFieldSortOptions)

instance Data.FromJSON PivotFieldSortOptions where
  parseJSON =
    Data.withObject
      "PivotFieldSortOptions"
      ( \x ->
          PivotFieldSortOptions'
            Prelude.<$> (x Data..: "FieldId")
            Prelude.<*> (x Data..: "SortBy")
      )

instance Prelude.Hashable PivotFieldSortOptions where
  hashWithSalt _salt PivotFieldSortOptions' {..} =
    _salt `Prelude.hashWithSalt` fieldId
      `Prelude.hashWithSalt` sortBy

instance Prelude.NFData PivotFieldSortOptions where
  rnf PivotFieldSortOptions' {..} =
    Prelude.rnf fieldId
      `Prelude.seq` Prelude.rnf sortBy

instance Data.ToJSON PivotFieldSortOptions where
  toJSON PivotFieldSortOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FieldId" Data..= fieldId),
            Prelude.Just ("SortBy" Data..= sortBy)
          ]
      )
