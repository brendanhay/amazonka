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
-- Module      : Amazonka.HoneyCode.Types.ResultSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.ResultSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types.ColumnMetadata
import Amazonka.HoneyCode.Types.ResultRow
import qualified Amazonka.Prelude as Prelude

-- | ResultSet contains the results of the request for a single block or list
-- defined on the screen.
--
-- /See:/ 'newResultSet' smart constructor.
data ResultSet = ResultSet'
  { -- | List of headers for all the data cells in the block. The header
    -- identifies the name and default format of the data cell. Data cells
    -- appear in the same order in all rows as defined in the header. The names
    -- and formats are not repeated in the rows. If a particular row does not
    -- have a value for a data cell, a blank value is used.
    --
    -- For example, a task list that displays the task name, due date and
    -- assigned person might have headers [ { \"name\": \"Task Name\"},
    -- {\"name\": \"Due Date\", \"format\": \"DATE\"}, {\"name\": \"Assigned\",
    -- \"format\": \"CONTACT\"} ]. Every row in the result will have the task
    -- name as the first item, due date as the second item and assigned person
    -- as the third item. If a particular task does not have a due date, that
    -- row will still have a blank value in the second element and the assigned
    -- person will still be in the third element.
    headers :: [ColumnMetadata],
    -- | List of rows returned by the request. Each row has a row Id and a list
    -- of data cells in that row. The data cells will be present in the same
    -- order as they are defined in the header.
    rows :: [ResultRow]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headers', 'resultSet_headers' - List of headers for all the data cells in the block. The header
-- identifies the name and default format of the data cell. Data cells
-- appear in the same order in all rows as defined in the header. The names
-- and formats are not repeated in the rows. If a particular row does not
-- have a value for a data cell, a blank value is used.
--
-- For example, a task list that displays the task name, due date and
-- assigned person might have headers [ { \"name\": \"Task Name\"},
-- {\"name\": \"Due Date\", \"format\": \"DATE\"}, {\"name\": \"Assigned\",
-- \"format\": \"CONTACT\"} ]. Every row in the result will have the task
-- name as the first item, due date as the second item and assigned person
-- as the third item. If a particular task does not have a due date, that
-- row will still have a blank value in the second element and the assigned
-- person will still be in the third element.
--
-- 'rows', 'resultSet_rows' - List of rows returned by the request. Each row has a row Id and a list
-- of data cells in that row. The data cells will be present in the same
-- order as they are defined in the header.
newResultSet ::
  ResultSet
newResultSet =
  ResultSet'
    { headers = Prelude.mempty,
      rows = Prelude.mempty
    }

-- | List of headers for all the data cells in the block. The header
-- identifies the name and default format of the data cell. Data cells
-- appear in the same order in all rows as defined in the header. The names
-- and formats are not repeated in the rows. If a particular row does not
-- have a value for a data cell, a blank value is used.
--
-- For example, a task list that displays the task name, due date and
-- assigned person might have headers [ { \"name\": \"Task Name\"},
-- {\"name\": \"Due Date\", \"format\": \"DATE\"}, {\"name\": \"Assigned\",
-- \"format\": \"CONTACT\"} ]. Every row in the result will have the task
-- name as the first item, due date as the second item and assigned person
-- as the third item. If a particular task does not have a due date, that
-- row will still have a blank value in the second element and the assigned
-- person will still be in the third element.
resultSet_headers :: Lens.Lens' ResultSet [ColumnMetadata]
resultSet_headers = Lens.lens (\ResultSet' {headers} -> headers) (\s@ResultSet' {} a -> s {headers = a} :: ResultSet) Prelude.. Lens.coerced

-- | List of rows returned by the request. Each row has a row Id and a list
-- of data cells in that row. The data cells will be present in the same
-- order as they are defined in the header.
resultSet_rows :: Lens.Lens' ResultSet [ResultRow]
resultSet_rows = Lens.lens (\ResultSet' {rows} -> rows) (\s@ResultSet' {} a -> s {rows = a} :: ResultSet) Prelude.. Lens.coerced

instance Data.FromJSON ResultSet where
  parseJSON =
    Data.withObject
      "ResultSet"
      ( \x ->
          ResultSet'
            Prelude.<$> (x Data..:? "headers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "rows" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResultSet where
  hashWithSalt _salt ResultSet' {..} =
    _salt
      `Prelude.hashWithSalt` headers
      `Prelude.hashWithSalt` rows

instance Prelude.NFData ResultSet where
  rnf ResultSet' {..} =
    Prelude.rnf headers `Prelude.seq` Prelude.rnf rows
