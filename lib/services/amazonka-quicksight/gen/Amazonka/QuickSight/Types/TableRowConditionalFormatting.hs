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
-- Module      : Amazonka.QuickSight.Types.TableRowConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableRowConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingColor

-- | The conditional formatting of a table row.
--
-- /See:/ 'newTableRowConditionalFormatting' smart constructor.
data TableRowConditionalFormatting = TableRowConditionalFormatting'
  { -- | The conditional formatting color (solid, gradient) of the background for
    -- a table row.
    backgroundColor :: Prelude.Maybe ConditionalFormattingColor,
    -- | The conditional formatting color (solid, gradient) of the text for a
    -- table row.
    textColor :: Prelude.Maybe ConditionalFormattingColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableRowConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundColor', 'tableRowConditionalFormatting_backgroundColor' - The conditional formatting color (solid, gradient) of the background for
-- a table row.
--
-- 'textColor', 'tableRowConditionalFormatting_textColor' - The conditional formatting color (solid, gradient) of the text for a
-- table row.
newTableRowConditionalFormatting ::
  TableRowConditionalFormatting
newTableRowConditionalFormatting =
  TableRowConditionalFormatting'
    { backgroundColor =
        Prelude.Nothing,
      textColor = Prelude.Nothing
    }

-- | The conditional formatting color (solid, gradient) of the background for
-- a table row.
tableRowConditionalFormatting_backgroundColor :: Lens.Lens' TableRowConditionalFormatting (Prelude.Maybe ConditionalFormattingColor)
tableRowConditionalFormatting_backgroundColor = Lens.lens (\TableRowConditionalFormatting' {backgroundColor} -> backgroundColor) (\s@TableRowConditionalFormatting' {} a -> s {backgroundColor = a} :: TableRowConditionalFormatting)

-- | The conditional formatting color (solid, gradient) of the text for a
-- table row.
tableRowConditionalFormatting_textColor :: Lens.Lens' TableRowConditionalFormatting (Prelude.Maybe ConditionalFormattingColor)
tableRowConditionalFormatting_textColor = Lens.lens (\TableRowConditionalFormatting' {textColor} -> textColor) (\s@TableRowConditionalFormatting' {} a -> s {textColor = a} :: TableRowConditionalFormatting)

instance Data.FromJSON TableRowConditionalFormatting where
  parseJSON =
    Data.withObject
      "TableRowConditionalFormatting"
      ( \x ->
          TableRowConditionalFormatting'
            Prelude.<$> (x Data..:? "BackgroundColor")
            Prelude.<*> (x Data..:? "TextColor")
      )

instance
  Prelude.Hashable
    TableRowConditionalFormatting
  where
  hashWithSalt _salt TableRowConditionalFormatting' {..} =
    _salt
      `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` textColor

instance Prelude.NFData TableRowConditionalFormatting where
  rnf TableRowConditionalFormatting' {..} =
    Prelude.rnf backgroundColor `Prelude.seq`
      Prelude.rnf textColor

instance Data.ToJSON TableRowConditionalFormatting where
  toJSON TableRowConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("TextColor" Data..=) Prelude.<$> textColor
          ]
      )
