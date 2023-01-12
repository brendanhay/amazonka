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
-- Module      : Amazonka.QuickSight.Types.PivotTableConditionalFormattingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableConditionalFormattingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableCellConditionalFormatting

-- | Conditional formatting options for a @PivotTableVisual@.
--
-- /See:/ 'newPivotTableConditionalFormattingOption' smart constructor.
data PivotTableConditionalFormattingOption = PivotTableConditionalFormattingOption'
  { -- | The cell conditional formatting option for a pivot table.
    cell :: Prelude.Maybe PivotTableCellConditionalFormatting
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableConditionalFormattingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cell', 'pivotTableConditionalFormattingOption_cell' - The cell conditional formatting option for a pivot table.
newPivotTableConditionalFormattingOption ::
  PivotTableConditionalFormattingOption
newPivotTableConditionalFormattingOption =
  PivotTableConditionalFormattingOption'
    { cell =
        Prelude.Nothing
    }

-- | The cell conditional formatting option for a pivot table.
pivotTableConditionalFormattingOption_cell :: Lens.Lens' PivotTableConditionalFormattingOption (Prelude.Maybe PivotTableCellConditionalFormatting)
pivotTableConditionalFormattingOption_cell = Lens.lens (\PivotTableConditionalFormattingOption' {cell} -> cell) (\s@PivotTableConditionalFormattingOption' {} a -> s {cell = a} :: PivotTableConditionalFormattingOption)

instance
  Data.FromJSON
    PivotTableConditionalFormattingOption
  where
  parseJSON =
    Data.withObject
      "PivotTableConditionalFormattingOption"
      ( \x ->
          PivotTableConditionalFormattingOption'
            Prelude.<$> (x Data..:? "Cell")
      )

instance
  Prelude.Hashable
    PivotTableConditionalFormattingOption
  where
  hashWithSalt
    _salt
    PivotTableConditionalFormattingOption' {..} =
      _salt `Prelude.hashWithSalt` cell

instance
  Prelude.NFData
    PivotTableConditionalFormattingOption
  where
  rnf PivotTableConditionalFormattingOption' {..} =
    Prelude.rnf cell

instance
  Data.ToJSON
    PivotTableConditionalFormattingOption
  where
  toJSON PivotTableConditionalFormattingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Cell" Data..=) Prelude.<$> cell]
      )
