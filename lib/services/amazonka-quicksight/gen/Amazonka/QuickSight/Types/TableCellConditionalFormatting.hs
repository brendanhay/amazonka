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
-- Module      : Amazonka.QuickSight.Types.TableCellConditionalFormatting
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableCellConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TextConditionalFormat

-- | The cell conditional formatting option for a table.
--
-- /See:/ 'newTableCellConditionalFormatting' smart constructor.
data TableCellConditionalFormatting = TableCellConditionalFormatting'
  { -- | The text format of the cell for conditional formatting.
    textFormat :: Prelude.Maybe TextConditionalFormat,
    -- | The field ID of the cell for conditional formatting.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableCellConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textFormat', 'tableCellConditionalFormatting_textFormat' - The text format of the cell for conditional formatting.
--
-- 'fieldId', 'tableCellConditionalFormatting_fieldId' - The field ID of the cell for conditional formatting.
newTableCellConditionalFormatting ::
  -- | 'fieldId'
  Prelude.Text ->
  TableCellConditionalFormatting
newTableCellConditionalFormatting pFieldId_ =
  TableCellConditionalFormatting'
    { textFormat =
        Prelude.Nothing,
      fieldId = pFieldId_
    }

-- | The text format of the cell for conditional formatting.
tableCellConditionalFormatting_textFormat :: Lens.Lens' TableCellConditionalFormatting (Prelude.Maybe TextConditionalFormat)
tableCellConditionalFormatting_textFormat = Lens.lens (\TableCellConditionalFormatting' {textFormat} -> textFormat) (\s@TableCellConditionalFormatting' {} a -> s {textFormat = a} :: TableCellConditionalFormatting)

-- | The field ID of the cell for conditional formatting.
tableCellConditionalFormatting_fieldId :: Lens.Lens' TableCellConditionalFormatting Prelude.Text
tableCellConditionalFormatting_fieldId = Lens.lens (\TableCellConditionalFormatting' {fieldId} -> fieldId) (\s@TableCellConditionalFormatting' {} a -> s {fieldId = a} :: TableCellConditionalFormatting)

instance Data.FromJSON TableCellConditionalFormatting where
  parseJSON =
    Data.withObject
      "TableCellConditionalFormatting"
      ( \x ->
          TableCellConditionalFormatting'
            Prelude.<$> (x Data..:? "TextFormat")
            Prelude.<*> (x Data..: "FieldId")
      )

instance
  Prelude.Hashable
    TableCellConditionalFormatting
  where
  hashWithSalt
    _salt
    TableCellConditionalFormatting' {..} =
      _salt `Prelude.hashWithSalt` textFormat
        `Prelude.hashWithSalt` fieldId

instance
  Prelude.NFData
    TableCellConditionalFormatting
  where
  rnf TableCellConditionalFormatting' {..} =
    Prelude.rnf textFormat
      `Prelude.seq` Prelude.rnf fieldId

instance Data.ToJSON TableCellConditionalFormatting where
  toJSON TableCellConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TextFormat" Data..=) Prelude.<$> textFormat,
            Prelude.Just ("FieldId" Data..= fieldId)
          ]
      )
