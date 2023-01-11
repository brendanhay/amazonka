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
-- Module      : Amazonka.Kendra.Types.TableCell
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.TableCell where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a table cell in a table excerpt.
--
-- /See:/ 'newTableCell' smart constructor.
data TableCell = TableCell'
  { -- | @TRUE@ means that the table cell should be treated as a header.
    header :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ means that the table cell has a high enough confidence and is
    -- relevant to the query, so the value or content should be highlighted.
    highlighted :: Prelude.Maybe Prelude.Bool,
    -- | @TRUE@ if the response of the table cell is the top answer. This is the
    -- cell value or content with the highest confidence score or is the most
    -- relevant to the query.
    topAnswer :: Prelude.Maybe Prelude.Bool,
    -- | The actual value or content within a table cell. A table cell could
    -- contain a date value of a year, or a string value of text, for example.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableCell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'header', 'tableCell_header' - @TRUE@ means that the table cell should be treated as a header.
--
-- 'highlighted', 'tableCell_highlighted' - @TRUE@ means that the table cell has a high enough confidence and is
-- relevant to the query, so the value or content should be highlighted.
--
-- 'topAnswer', 'tableCell_topAnswer' - @TRUE@ if the response of the table cell is the top answer. This is the
-- cell value or content with the highest confidence score or is the most
-- relevant to the query.
--
-- 'value', 'tableCell_value' - The actual value or content within a table cell. A table cell could
-- contain a date value of a year, or a string value of text, for example.
newTableCell ::
  TableCell
newTableCell =
  TableCell'
    { header = Prelude.Nothing,
      highlighted = Prelude.Nothing,
      topAnswer = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | @TRUE@ means that the table cell should be treated as a header.
tableCell_header :: Lens.Lens' TableCell (Prelude.Maybe Prelude.Bool)
tableCell_header = Lens.lens (\TableCell' {header} -> header) (\s@TableCell' {} a -> s {header = a} :: TableCell)

-- | @TRUE@ means that the table cell has a high enough confidence and is
-- relevant to the query, so the value or content should be highlighted.
tableCell_highlighted :: Lens.Lens' TableCell (Prelude.Maybe Prelude.Bool)
tableCell_highlighted = Lens.lens (\TableCell' {highlighted} -> highlighted) (\s@TableCell' {} a -> s {highlighted = a} :: TableCell)

-- | @TRUE@ if the response of the table cell is the top answer. This is the
-- cell value or content with the highest confidence score or is the most
-- relevant to the query.
tableCell_topAnswer :: Lens.Lens' TableCell (Prelude.Maybe Prelude.Bool)
tableCell_topAnswer = Lens.lens (\TableCell' {topAnswer} -> topAnswer) (\s@TableCell' {} a -> s {topAnswer = a} :: TableCell)

-- | The actual value or content within a table cell. A table cell could
-- contain a date value of a year, or a string value of text, for example.
tableCell_value :: Lens.Lens' TableCell (Prelude.Maybe Prelude.Text)
tableCell_value = Lens.lens (\TableCell' {value} -> value) (\s@TableCell' {} a -> s {value = a} :: TableCell)

instance Data.FromJSON TableCell where
  parseJSON =
    Data.withObject
      "TableCell"
      ( \x ->
          TableCell'
            Prelude.<$> (x Data..:? "Header")
            Prelude.<*> (x Data..:? "Highlighted")
            Prelude.<*> (x Data..:? "TopAnswer")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable TableCell where
  hashWithSalt _salt TableCell' {..} =
    _salt `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` highlighted
      `Prelude.hashWithSalt` topAnswer
      `Prelude.hashWithSalt` value

instance Prelude.NFData TableCell where
  rnf TableCell' {..} =
    Prelude.rnf header
      `Prelude.seq` Prelude.rnf highlighted
      `Prelude.seq` Prelude.rnf topAnswer
      `Prelude.seq` Prelude.rnf value
