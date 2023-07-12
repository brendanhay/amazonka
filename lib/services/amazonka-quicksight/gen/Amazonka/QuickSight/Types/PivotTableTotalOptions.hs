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
-- Module      : Amazonka.QuickSight.Types.PivotTableTotalOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableTotalOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTotalOptions
import Amazonka.QuickSight.Types.SubtotalOptions

-- | The total options for a pivot table visual.
--
-- /See:/ 'newPivotTableTotalOptions' smart constructor.
data PivotTableTotalOptions = PivotTableTotalOptions'
  { -- | The column subtotal options.
    columnSubtotalOptions :: Prelude.Maybe SubtotalOptions,
    -- | The column total options.
    columnTotalOptions :: Prelude.Maybe PivotTotalOptions,
    -- | The row subtotal options.
    rowSubtotalOptions :: Prelude.Maybe SubtotalOptions,
    -- | The row total options.
    rowTotalOptions :: Prelude.Maybe PivotTotalOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableTotalOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnSubtotalOptions', 'pivotTableTotalOptions_columnSubtotalOptions' - The column subtotal options.
--
-- 'columnTotalOptions', 'pivotTableTotalOptions_columnTotalOptions' - The column total options.
--
-- 'rowSubtotalOptions', 'pivotTableTotalOptions_rowSubtotalOptions' - The row subtotal options.
--
-- 'rowTotalOptions', 'pivotTableTotalOptions_rowTotalOptions' - The row total options.
newPivotTableTotalOptions ::
  PivotTableTotalOptions
newPivotTableTotalOptions =
  PivotTableTotalOptions'
    { columnSubtotalOptions =
        Prelude.Nothing,
      columnTotalOptions = Prelude.Nothing,
      rowSubtotalOptions = Prelude.Nothing,
      rowTotalOptions = Prelude.Nothing
    }

-- | The column subtotal options.
pivotTableTotalOptions_columnSubtotalOptions :: Lens.Lens' PivotTableTotalOptions (Prelude.Maybe SubtotalOptions)
pivotTableTotalOptions_columnSubtotalOptions = Lens.lens (\PivotTableTotalOptions' {columnSubtotalOptions} -> columnSubtotalOptions) (\s@PivotTableTotalOptions' {} a -> s {columnSubtotalOptions = a} :: PivotTableTotalOptions)

-- | The column total options.
pivotTableTotalOptions_columnTotalOptions :: Lens.Lens' PivotTableTotalOptions (Prelude.Maybe PivotTotalOptions)
pivotTableTotalOptions_columnTotalOptions = Lens.lens (\PivotTableTotalOptions' {columnTotalOptions} -> columnTotalOptions) (\s@PivotTableTotalOptions' {} a -> s {columnTotalOptions = a} :: PivotTableTotalOptions)

-- | The row subtotal options.
pivotTableTotalOptions_rowSubtotalOptions :: Lens.Lens' PivotTableTotalOptions (Prelude.Maybe SubtotalOptions)
pivotTableTotalOptions_rowSubtotalOptions = Lens.lens (\PivotTableTotalOptions' {rowSubtotalOptions} -> rowSubtotalOptions) (\s@PivotTableTotalOptions' {} a -> s {rowSubtotalOptions = a} :: PivotTableTotalOptions)

-- | The row total options.
pivotTableTotalOptions_rowTotalOptions :: Lens.Lens' PivotTableTotalOptions (Prelude.Maybe PivotTotalOptions)
pivotTableTotalOptions_rowTotalOptions = Lens.lens (\PivotTableTotalOptions' {rowTotalOptions} -> rowTotalOptions) (\s@PivotTableTotalOptions' {} a -> s {rowTotalOptions = a} :: PivotTableTotalOptions)

instance Data.FromJSON PivotTableTotalOptions where
  parseJSON =
    Data.withObject
      "PivotTableTotalOptions"
      ( \x ->
          PivotTableTotalOptions'
            Prelude.<$> (x Data..:? "ColumnSubtotalOptions")
            Prelude.<*> (x Data..:? "ColumnTotalOptions")
            Prelude.<*> (x Data..:? "RowSubtotalOptions")
            Prelude.<*> (x Data..:? "RowTotalOptions")
      )

instance Prelude.Hashable PivotTableTotalOptions where
  hashWithSalt _salt PivotTableTotalOptions' {..} =
    _salt
      `Prelude.hashWithSalt` columnSubtotalOptions
      `Prelude.hashWithSalt` columnTotalOptions
      `Prelude.hashWithSalt` rowSubtotalOptions
      `Prelude.hashWithSalt` rowTotalOptions

instance Prelude.NFData PivotTableTotalOptions where
  rnf PivotTableTotalOptions' {..} =
    Prelude.rnf columnSubtotalOptions
      `Prelude.seq` Prelude.rnf columnTotalOptions
      `Prelude.seq` Prelude.rnf rowSubtotalOptions
      `Prelude.seq` Prelude.rnf rowTotalOptions

instance Data.ToJSON PivotTableTotalOptions where
  toJSON PivotTableTotalOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColumnSubtotalOptions" Data..=)
              Prelude.<$> columnSubtotalOptions,
            ("ColumnTotalOptions" Data..=)
              Prelude.<$> columnTotalOptions,
            ("RowSubtotalOptions" Data..=)
              Prelude.<$> rowSubtotalOptions,
            ("RowTotalOptions" Data..=)
              Prelude.<$> rowTotalOptions
          ]
      )
