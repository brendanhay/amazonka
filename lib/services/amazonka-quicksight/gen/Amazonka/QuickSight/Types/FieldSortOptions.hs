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
-- Module      : Amazonka.QuickSight.Types.FieldSortOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FieldSortOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnSort
import Amazonka.QuickSight.Types.FieldSort

-- | The field sort options in a chart configuration.
--
-- /See:/ 'newFieldSortOptions' smart constructor.
data FieldSortOptions = FieldSortOptions'
  { -- | The sort configuration for a column that is not used in a field well.
    columnSort :: Prelude.Maybe ColumnSort,
    -- | The sort configuration for a field in a field well.
    fieldSort :: Prelude.Maybe FieldSort
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldSortOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnSort', 'fieldSortOptions_columnSort' - The sort configuration for a column that is not used in a field well.
--
-- 'fieldSort', 'fieldSortOptions_fieldSort' - The sort configuration for a field in a field well.
newFieldSortOptions ::
  FieldSortOptions
newFieldSortOptions =
  FieldSortOptions'
    { columnSort = Prelude.Nothing,
      fieldSort = Prelude.Nothing
    }

-- | The sort configuration for a column that is not used in a field well.
fieldSortOptions_columnSort :: Lens.Lens' FieldSortOptions (Prelude.Maybe ColumnSort)
fieldSortOptions_columnSort = Lens.lens (\FieldSortOptions' {columnSort} -> columnSort) (\s@FieldSortOptions' {} a -> s {columnSort = a} :: FieldSortOptions)

-- | The sort configuration for a field in a field well.
fieldSortOptions_fieldSort :: Lens.Lens' FieldSortOptions (Prelude.Maybe FieldSort)
fieldSortOptions_fieldSort = Lens.lens (\FieldSortOptions' {fieldSort} -> fieldSort) (\s@FieldSortOptions' {} a -> s {fieldSort = a} :: FieldSortOptions)

instance Data.FromJSON FieldSortOptions where
  parseJSON =
    Data.withObject
      "FieldSortOptions"
      ( \x ->
          FieldSortOptions'
            Prelude.<$> (x Data..:? "ColumnSort")
            Prelude.<*> (x Data..:? "FieldSort")
      )

instance Prelude.Hashable FieldSortOptions where
  hashWithSalt _salt FieldSortOptions' {..} =
    _salt
      `Prelude.hashWithSalt` columnSort
      `Prelude.hashWithSalt` fieldSort

instance Prelude.NFData FieldSortOptions where
  rnf FieldSortOptions' {..} =
    Prelude.rnf columnSort `Prelude.seq`
      Prelude.rnf fieldSort

instance Data.ToJSON FieldSortOptions where
  toJSON FieldSortOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnSort" Data..=) Prelude.<$> columnSort,
            ("FieldSort" Data..=) Prelude.<$> fieldSort
          ]
      )
