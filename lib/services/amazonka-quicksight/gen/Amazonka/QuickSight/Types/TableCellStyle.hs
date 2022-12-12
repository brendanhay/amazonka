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
-- Module      : Amazonka.QuickSight.Types.TableCellStyle
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableCellStyle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FontConfiguration
import Amazonka.QuickSight.Types.GlobalTableBorderOptions
import Amazonka.QuickSight.Types.HorizontalTextAlignment
import Amazonka.QuickSight.Types.TextWrap
import Amazonka.QuickSight.Types.VerticalTextAlignment
import Amazonka.QuickSight.Types.Visibility

-- | The table cell style for a cell in pivot table or table visual.
--
-- /See:/ 'newTableCellStyle' smart constructor.
data TableCellStyle = TableCellStyle'
  { -- | The background color for the table cells.
    backgroundColor :: Prelude.Maybe Prelude.Text,
    -- | The borders for the table cells.
    border :: Prelude.Maybe GlobalTableBorderOptions,
    -- | The font configuration of the table cells.
    fontConfiguration :: Prelude.Maybe FontConfiguration,
    -- | The height color for the table cells.
    height :: Prelude.Maybe Prelude.Natural,
    -- | The horizontal text alignment (left, center, right, auto) for the table
    -- cells.
    horizontalTextAlignment :: Prelude.Maybe HorizontalTextAlignment,
    -- | The text wrap (none, wrap) for the table cells.
    textWrap :: Prelude.Maybe TextWrap,
    -- | The vertical text alignment (top, middle, bottom) for the table cells.
    verticalTextAlignment :: Prelude.Maybe VerticalTextAlignment,
    -- | The visibility of the table cells.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableCellStyle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundColor', 'tableCellStyle_backgroundColor' - The background color for the table cells.
--
-- 'border', 'tableCellStyle_border' - The borders for the table cells.
--
-- 'fontConfiguration', 'tableCellStyle_fontConfiguration' - The font configuration of the table cells.
--
-- 'height', 'tableCellStyle_height' - The height color for the table cells.
--
-- 'horizontalTextAlignment', 'tableCellStyle_horizontalTextAlignment' - The horizontal text alignment (left, center, right, auto) for the table
-- cells.
--
-- 'textWrap', 'tableCellStyle_textWrap' - The text wrap (none, wrap) for the table cells.
--
-- 'verticalTextAlignment', 'tableCellStyle_verticalTextAlignment' - The vertical text alignment (top, middle, bottom) for the table cells.
--
-- 'visibility', 'tableCellStyle_visibility' - The visibility of the table cells.
newTableCellStyle ::
  TableCellStyle
newTableCellStyle =
  TableCellStyle'
    { backgroundColor = Prelude.Nothing,
      border = Prelude.Nothing,
      fontConfiguration = Prelude.Nothing,
      height = Prelude.Nothing,
      horizontalTextAlignment = Prelude.Nothing,
      textWrap = Prelude.Nothing,
      verticalTextAlignment = Prelude.Nothing,
      visibility = Prelude.Nothing
    }

-- | The background color for the table cells.
tableCellStyle_backgroundColor :: Lens.Lens' TableCellStyle (Prelude.Maybe Prelude.Text)
tableCellStyle_backgroundColor = Lens.lens (\TableCellStyle' {backgroundColor} -> backgroundColor) (\s@TableCellStyle' {} a -> s {backgroundColor = a} :: TableCellStyle)

-- | The borders for the table cells.
tableCellStyle_border :: Lens.Lens' TableCellStyle (Prelude.Maybe GlobalTableBorderOptions)
tableCellStyle_border = Lens.lens (\TableCellStyle' {border} -> border) (\s@TableCellStyle' {} a -> s {border = a} :: TableCellStyle)

-- | The font configuration of the table cells.
tableCellStyle_fontConfiguration :: Lens.Lens' TableCellStyle (Prelude.Maybe FontConfiguration)
tableCellStyle_fontConfiguration = Lens.lens (\TableCellStyle' {fontConfiguration} -> fontConfiguration) (\s@TableCellStyle' {} a -> s {fontConfiguration = a} :: TableCellStyle)

-- | The height color for the table cells.
tableCellStyle_height :: Lens.Lens' TableCellStyle (Prelude.Maybe Prelude.Natural)
tableCellStyle_height = Lens.lens (\TableCellStyle' {height} -> height) (\s@TableCellStyle' {} a -> s {height = a} :: TableCellStyle)

-- | The horizontal text alignment (left, center, right, auto) for the table
-- cells.
tableCellStyle_horizontalTextAlignment :: Lens.Lens' TableCellStyle (Prelude.Maybe HorizontalTextAlignment)
tableCellStyle_horizontalTextAlignment = Lens.lens (\TableCellStyle' {horizontalTextAlignment} -> horizontalTextAlignment) (\s@TableCellStyle' {} a -> s {horizontalTextAlignment = a} :: TableCellStyle)

-- | The text wrap (none, wrap) for the table cells.
tableCellStyle_textWrap :: Lens.Lens' TableCellStyle (Prelude.Maybe TextWrap)
tableCellStyle_textWrap = Lens.lens (\TableCellStyle' {textWrap} -> textWrap) (\s@TableCellStyle' {} a -> s {textWrap = a} :: TableCellStyle)

-- | The vertical text alignment (top, middle, bottom) for the table cells.
tableCellStyle_verticalTextAlignment :: Lens.Lens' TableCellStyle (Prelude.Maybe VerticalTextAlignment)
tableCellStyle_verticalTextAlignment = Lens.lens (\TableCellStyle' {verticalTextAlignment} -> verticalTextAlignment) (\s@TableCellStyle' {} a -> s {verticalTextAlignment = a} :: TableCellStyle)

-- | The visibility of the table cells.
tableCellStyle_visibility :: Lens.Lens' TableCellStyle (Prelude.Maybe Visibility)
tableCellStyle_visibility = Lens.lens (\TableCellStyle' {visibility} -> visibility) (\s@TableCellStyle' {} a -> s {visibility = a} :: TableCellStyle)

instance Data.FromJSON TableCellStyle where
  parseJSON =
    Data.withObject
      "TableCellStyle"
      ( \x ->
          TableCellStyle'
            Prelude.<$> (x Data..:? "BackgroundColor")
            Prelude.<*> (x Data..:? "Border")
            Prelude.<*> (x Data..:? "FontConfiguration")
            Prelude.<*> (x Data..:? "Height")
            Prelude.<*> (x Data..:? "HorizontalTextAlignment")
            Prelude.<*> (x Data..:? "TextWrap")
            Prelude.<*> (x Data..:? "VerticalTextAlignment")
            Prelude.<*> (x Data..:? "Visibility")
      )

instance Prelude.Hashable TableCellStyle where
  hashWithSalt _salt TableCellStyle' {..} =
    _salt `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` border
      `Prelude.hashWithSalt` fontConfiguration
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` horizontalTextAlignment
      `Prelude.hashWithSalt` textWrap
      `Prelude.hashWithSalt` verticalTextAlignment
      `Prelude.hashWithSalt` visibility

instance Prelude.NFData TableCellStyle where
  rnf TableCellStyle' {..} =
    Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf border
      `Prelude.seq` Prelude.rnf fontConfiguration
      `Prelude.seq` Prelude.rnf height
      `Prelude.seq` Prelude.rnf horizontalTextAlignment
      `Prelude.seq` Prelude.rnf textWrap
      `Prelude.seq` Prelude.rnf verticalTextAlignment
      `Prelude.seq` Prelude.rnf visibility

instance Data.ToJSON TableCellStyle where
  toJSON TableCellStyle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("Border" Data..=) Prelude.<$> border,
            ("FontConfiguration" Data..=)
              Prelude.<$> fontConfiguration,
            ("Height" Data..=) Prelude.<$> height,
            ("HorizontalTextAlignment" Data..=)
              Prelude.<$> horizontalTextAlignment,
            ("TextWrap" Data..=) Prelude.<$> textWrap,
            ("VerticalTextAlignment" Data..=)
              Prelude.<$> verticalTextAlignment,
            ("Visibility" Data..=) Prelude.<$> visibility
          ]
      )
