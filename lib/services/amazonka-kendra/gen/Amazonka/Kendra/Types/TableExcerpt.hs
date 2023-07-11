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
-- Module      : Amazonka.Kendra.Types.TableExcerpt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.TableExcerpt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.TableRow
import qualified Amazonka.Prelude as Prelude

-- | An excerpt from a table within a document. The table excerpt displays up
-- to five columns and three rows, depending on how many table cells are
-- relevant to the query and how many columns are available in the original
-- table. The top most relevant cell is displayed in the table excerpt,
-- along with the next most relevant cells.
--
-- /See:/ 'newTableExcerpt' smart constructor.
data TableExcerpt = TableExcerpt'
  { -- | A list of rows in the table excerpt.
    rows :: Prelude.Maybe [TableRow],
    -- | A count of the number of rows in the original table within the document.
    totalNumberOfRows :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableExcerpt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rows', 'tableExcerpt_rows' - A list of rows in the table excerpt.
--
-- 'totalNumberOfRows', 'tableExcerpt_totalNumberOfRows' - A count of the number of rows in the original table within the document.
newTableExcerpt ::
  TableExcerpt
newTableExcerpt =
  TableExcerpt'
    { rows = Prelude.Nothing,
      totalNumberOfRows = Prelude.Nothing
    }

-- | A list of rows in the table excerpt.
tableExcerpt_rows :: Lens.Lens' TableExcerpt (Prelude.Maybe [TableRow])
tableExcerpt_rows = Lens.lens (\TableExcerpt' {rows} -> rows) (\s@TableExcerpt' {} a -> s {rows = a} :: TableExcerpt) Prelude.. Lens.mapping Lens.coerced

-- | A count of the number of rows in the original table within the document.
tableExcerpt_totalNumberOfRows :: Lens.Lens' TableExcerpt (Prelude.Maybe Prelude.Int)
tableExcerpt_totalNumberOfRows = Lens.lens (\TableExcerpt' {totalNumberOfRows} -> totalNumberOfRows) (\s@TableExcerpt' {} a -> s {totalNumberOfRows = a} :: TableExcerpt)

instance Data.FromJSON TableExcerpt where
  parseJSON =
    Data.withObject
      "TableExcerpt"
      ( \x ->
          TableExcerpt'
            Prelude.<$> (x Data..:? "Rows" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TotalNumberOfRows")
      )

instance Prelude.Hashable TableExcerpt where
  hashWithSalt _salt TableExcerpt' {..} =
    _salt
      `Prelude.hashWithSalt` rows
      `Prelude.hashWithSalt` totalNumberOfRows

instance Prelude.NFData TableExcerpt where
  rnf TableExcerpt' {..} =
    Prelude.rnf rows
      `Prelude.seq` Prelude.rnf totalNumberOfRows
