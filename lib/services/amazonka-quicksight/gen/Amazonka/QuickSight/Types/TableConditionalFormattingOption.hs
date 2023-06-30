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
-- Module      : Amazonka.QuickSight.Types.TableConditionalFormattingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableConditionalFormattingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableCellConditionalFormatting
import Amazonka.QuickSight.Types.TableRowConditionalFormatting

-- | Conditional formatting options for a @PivotTableVisual@.
--
-- /See:/ 'newTableConditionalFormattingOption' smart constructor.
data TableConditionalFormattingOption = TableConditionalFormattingOption'
  { -- | The cell conditional formatting option for a table.
    cell :: Prelude.Maybe TableCellConditionalFormatting,
    -- | The row conditional formatting option for a table.
    row :: Prelude.Maybe TableRowConditionalFormatting
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableConditionalFormattingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cell', 'tableConditionalFormattingOption_cell' - The cell conditional formatting option for a table.
--
-- 'row', 'tableConditionalFormattingOption_row' - The row conditional formatting option for a table.
newTableConditionalFormattingOption ::
  TableConditionalFormattingOption
newTableConditionalFormattingOption =
  TableConditionalFormattingOption'
    { cell =
        Prelude.Nothing,
      row = Prelude.Nothing
    }

-- | The cell conditional formatting option for a table.
tableConditionalFormattingOption_cell :: Lens.Lens' TableConditionalFormattingOption (Prelude.Maybe TableCellConditionalFormatting)
tableConditionalFormattingOption_cell = Lens.lens (\TableConditionalFormattingOption' {cell} -> cell) (\s@TableConditionalFormattingOption' {} a -> s {cell = a} :: TableConditionalFormattingOption)

-- | The row conditional formatting option for a table.
tableConditionalFormattingOption_row :: Lens.Lens' TableConditionalFormattingOption (Prelude.Maybe TableRowConditionalFormatting)
tableConditionalFormattingOption_row = Lens.lens (\TableConditionalFormattingOption' {row} -> row) (\s@TableConditionalFormattingOption' {} a -> s {row = a} :: TableConditionalFormattingOption)

instance
  Data.FromJSON
    TableConditionalFormattingOption
  where
  parseJSON =
    Data.withObject
      "TableConditionalFormattingOption"
      ( \x ->
          TableConditionalFormattingOption'
            Prelude.<$> (x Data..:? "Cell")
            Prelude.<*> (x Data..:? "Row")
      )

instance
  Prelude.Hashable
    TableConditionalFormattingOption
  where
  hashWithSalt
    _salt
    TableConditionalFormattingOption' {..} =
      _salt
        `Prelude.hashWithSalt` cell
        `Prelude.hashWithSalt` row

instance
  Prelude.NFData
    TableConditionalFormattingOption
  where
  rnf TableConditionalFormattingOption' {..} =
    Prelude.rnf cell `Prelude.seq` Prelude.rnf row

instance Data.ToJSON TableConditionalFormattingOption where
  toJSON TableConditionalFormattingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Cell" Data..=) Prelude.<$> cell,
            ("Row" Data..=) Prelude.<$> row
          ]
      )
