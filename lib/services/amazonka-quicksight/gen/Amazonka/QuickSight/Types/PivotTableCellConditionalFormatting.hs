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
-- Module      : Amazonka.QuickSight.Types.PivotTableCellConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableCellConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableConditionalFormattingScope
import Amazonka.QuickSight.Types.TextConditionalFormat

-- | The cell conditional formatting option for a pivot table.
--
-- /See:/ 'newPivotTableCellConditionalFormatting' smart constructor.
data PivotTableCellConditionalFormatting = PivotTableCellConditionalFormatting'
  { -- | The scope of the cell for conditional formatting.
    scope :: Prelude.Maybe PivotTableConditionalFormattingScope,
    -- | A list of cell scopes for conditional formatting.
    scopes :: Prelude.Maybe [PivotTableConditionalFormattingScope],
    -- | The text format of the cell for conditional formatting.
    textFormat :: Prelude.Maybe TextConditionalFormat,
    -- | The field ID of the cell for conditional formatting.
    fieldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableCellConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'pivotTableCellConditionalFormatting_scope' - The scope of the cell for conditional formatting.
--
-- 'scopes', 'pivotTableCellConditionalFormatting_scopes' - A list of cell scopes for conditional formatting.
--
-- 'textFormat', 'pivotTableCellConditionalFormatting_textFormat' - The text format of the cell for conditional formatting.
--
-- 'fieldId', 'pivotTableCellConditionalFormatting_fieldId' - The field ID of the cell for conditional formatting.
newPivotTableCellConditionalFormatting ::
  -- | 'fieldId'
  Prelude.Text ->
  PivotTableCellConditionalFormatting
newPivotTableCellConditionalFormatting pFieldId_ =
  PivotTableCellConditionalFormatting'
    { scope =
        Prelude.Nothing,
      scopes = Prelude.Nothing,
      textFormat = Prelude.Nothing,
      fieldId = pFieldId_
    }

-- | The scope of the cell for conditional formatting.
pivotTableCellConditionalFormatting_scope :: Lens.Lens' PivotTableCellConditionalFormatting (Prelude.Maybe PivotTableConditionalFormattingScope)
pivotTableCellConditionalFormatting_scope = Lens.lens (\PivotTableCellConditionalFormatting' {scope} -> scope) (\s@PivotTableCellConditionalFormatting' {} a -> s {scope = a} :: PivotTableCellConditionalFormatting)

-- | A list of cell scopes for conditional formatting.
pivotTableCellConditionalFormatting_scopes :: Lens.Lens' PivotTableCellConditionalFormatting (Prelude.Maybe [PivotTableConditionalFormattingScope])
pivotTableCellConditionalFormatting_scopes = Lens.lens (\PivotTableCellConditionalFormatting' {scopes} -> scopes) (\s@PivotTableCellConditionalFormatting' {} a -> s {scopes = a} :: PivotTableCellConditionalFormatting) Prelude.. Lens.mapping Lens.coerced

-- | The text format of the cell for conditional formatting.
pivotTableCellConditionalFormatting_textFormat :: Lens.Lens' PivotTableCellConditionalFormatting (Prelude.Maybe TextConditionalFormat)
pivotTableCellConditionalFormatting_textFormat = Lens.lens (\PivotTableCellConditionalFormatting' {textFormat} -> textFormat) (\s@PivotTableCellConditionalFormatting' {} a -> s {textFormat = a} :: PivotTableCellConditionalFormatting)

-- | The field ID of the cell for conditional formatting.
pivotTableCellConditionalFormatting_fieldId :: Lens.Lens' PivotTableCellConditionalFormatting Prelude.Text
pivotTableCellConditionalFormatting_fieldId = Lens.lens (\PivotTableCellConditionalFormatting' {fieldId} -> fieldId) (\s@PivotTableCellConditionalFormatting' {} a -> s {fieldId = a} :: PivotTableCellConditionalFormatting)

instance
  Data.FromJSON
    PivotTableCellConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "PivotTableCellConditionalFormatting"
      ( \x ->
          PivotTableCellConditionalFormatting'
            Prelude.<$> (x Data..:? "Scope")
            Prelude.<*> (x Data..:? "Scopes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TextFormat")
            Prelude.<*> (x Data..: "FieldId")
      )

instance
  Prelude.Hashable
    PivotTableCellConditionalFormatting
  where
  hashWithSalt
    _salt
    PivotTableCellConditionalFormatting' {..} =
      _salt
        `Prelude.hashWithSalt` scope
        `Prelude.hashWithSalt` scopes
        `Prelude.hashWithSalt` textFormat
        `Prelude.hashWithSalt` fieldId

instance
  Prelude.NFData
    PivotTableCellConditionalFormatting
  where
  rnf PivotTableCellConditionalFormatting' {..} =
    Prelude.rnf scope
      `Prelude.seq` Prelude.rnf scopes
      `Prelude.seq` Prelude.rnf textFormat
      `Prelude.seq` Prelude.rnf fieldId

instance
  Data.ToJSON
    PivotTableCellConditionalFormatting
  where
  toJSON PivotTableCellConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Scope" Data..=) Prelude.<$> scope,
            ("Scopes" Data..=) Prelude.<$> scopes,
            ("TextFormat" Data..=) Prelude.<$> textFormat,
            Prelude.Just ("FieldId" Data..= fieldId)
          ]
      )
