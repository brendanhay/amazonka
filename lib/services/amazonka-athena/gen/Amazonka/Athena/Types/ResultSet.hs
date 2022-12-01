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
-- Module      : Amazonka.Athena.Types.ResultSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ResultSet where

import Amazonka.Athena.Types.ResultSetMetadata
import Amazonka.Athena.Types.Row
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The metadata and rows that make up a query result set. The metadata
-- describes the column structure and data types. To return a @ResultSet@
-- object, use GetQueryResults.
--
-- /See:/ 'newResultSet' smart constructor.
data ResultSet = ResultSet'
  { -- | The rows in the table.
    rows :: Prelude.Maybe [Row],
    -- | The metadata that describes the column structure and data types of a
    -- table of query results.
    resultSetMetadata :: Prelude.Maybe ResultSetMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rows', 'resultSet_rows' - The rows in the table.
--
-- 'resultSetMetadata', 'resultSet_resultSetMetadata' - The metadata that describes the column structure and data types of a
-- table of query results.
newResultSet ::
  ResultSet
newResultSet =
  ResultSet'
    { rows = Prelude.Nothing,
      resultSetMetadata = Prelude.Nothing
    }

-- | The rows in the table.
resultSet_rows :: Lens.Lens' ResultSet (Prelude.Maybe [Row])
resultSet_rows = Lens.lens (\ResultSet' {rows} -> rows) (\s@ResultSet' {} a -> s {rows = a} :: ResultSet) Prelude.. Lens.mapping Lens.coerced

-- | The metadata that describes the column structure and data types of a
-- table of query results.
resultSet_resultSetMetadata :: Lens.Lens' ResultSet (Prelude.Maybe ResultSetMetadata)
resultSet_resultSetMetadata = Lens.lens (\ResultSet' {resultSetMetadata} -> resultSetMetadata) (\s@ResultSet' {} a -> s {resultSetMetadata = a} :: ResultSet)

instance Core.FromJSON ResultSet where
  parseJSON =
    Core.withObject
      "ResultSet"
      ( \x ->
          ResultSet'
            Prelude.<$> (x Core..:? "Rows" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResultSetMetadata")
      )

instance Prelude.Hashable ResultSet where
  hashWithSalt _salt ResultSet' {..} =
    _salt `Prelude.hashWithSalt` rows
      `Prelude.hashWithSalt` resultSetMetadata

instance Prelude.NFData ResultSet where
  rnf ResultSet' {..} =
    Prelude.rnf rows
      `Prelude.seq` Prelude.rnf resultSetMetadata
