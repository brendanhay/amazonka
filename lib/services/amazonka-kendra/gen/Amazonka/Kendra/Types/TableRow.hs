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
-- Module      : Amazonka.Kendra.Types.TableRow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.TableRow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.TableCell
import qualified Amazonka.Prelude as Prelude

-- | Information about a row in a table excerpt.
--
-- /See:/ 'newTableRow' smart constructor.
data TableRow = TableRow'
  { -- | A list of table cells in a row.
    cells :: Prelude.Maybe [TableCell]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableRow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cells', 'tableRow_cells' - A list of table cells in a row.
newTableRow ::
  TableRow
newTableRow = TableRow' {cells = Prelude.Nothing}

-- | A list of table cells in a row.
tableRow_cells :: Lens.Lens' TableRow (Prelude.Maybe [TableCell])
tableRow_cells = Lens.lens (\TableRow' {cells} -> cells) (\s@TableRow' {} a -> s {cells = a} :: TableRow) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TableRow where
  parseJSON =
    Data.withObject
      "TableRow"
      ( \x ->
          TableRow'
            Prelude.<$> (x Data..:? "Cells" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable TableRow where
  hashWithSalt _salt TableRow' {..} =
    _salt `Prelude.hashWithSalt` cells

instance Prelude.NFData TableRow where
  rnf TableRow' {..} = Prelude.rnf cells
