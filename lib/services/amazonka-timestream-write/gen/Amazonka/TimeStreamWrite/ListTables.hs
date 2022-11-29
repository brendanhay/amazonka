{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.TimeStreamWrite.ListTables
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of tables, along with the name, status and retention properties
-- of each table. See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.list-table.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.ListTables
  ( -- * Creating a Request
    ListTables (..),
    newListTables,

    -- * Request Lenses
    listTables_nextToken,
    listTables_databaseName,
    listTables_maxResults,

    -- * Destructuring the Response
    ListTablesResponse (..),
    newListTablesResponse,

    -- * Response Lenses
    listTablesResponse_tables,
    listTablesResponse_nextToken,
    listTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newListTables' smart constructor.
data ListTables = ListTables'
  { -- | The pagination token. To resume pagination, provide the NextToken value
    -- as argument of a subsequent API invocation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the Timestream database.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The total number of items to return in the output. If the total number
    -- of items available is more than the value specified, a NextToken is
    -- provided in the output. To resume pagination, provide the NextToken
    -- value as argument of a subsequent API invocation.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTables_nextToken' - The pagination token. To resume pagination, provide the NextToken value
-- as argument of a subsequent API invocation.
--
-- 'databaseName', 'listTables_databaseName' - The name of the Timestream database.
--
-- 'maxResults', 'listTables_maxResults' - The total number of items to return in the output. If the total number
-- of items available is more than the value specified, a NextToken is
-- provided in the output. To resume pagination, provide the NextToken
-- value as argument of a subsequent API invocation.
newListTables ::
  ListTables
newListTables =
  ListTables'
    { nextToken = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token. To resume pagination, provide the NextToken value
-- as argument of a subsequent API invocation.
listTables_nextToken :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_nextToken = Lens.lens (\ListTables' {nextToken} -> nextToken) (\s@ListTables' {} a -> s {nextToken = a} :: ListTables)

-- | The name of the Timestream database.
listTables_databaseName :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_databaseName = Lens.lens (\ListTables' {databaseName} -> databaseName) (\s@ListTables' {} a -> s {databaseName = a} :: ListTables)

-- | The total number of items to return in the output. If the total number
-- of items available is more than the value specified, a NextToken is
-- provided in the output. To resume pagination, provide the NextToken
-- value as argument of a subsequent API invocation.
listTables_maxResults :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Natural)
listTables_maxResults = Lens.lens (\ListTables' {maxResults} -> maxResults) (\s@ListTables' {} a -> s {maxResults = a} :: ListTables)

instance Core.AWSRequest ListTables where
  type AWSResponse ListTables = ListTablesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTablesResponse'
            Prelude.<$> (x Core..?> "Tables" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTables where
  hashWithSalt _salt ListTables' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListTables where
  rnf ListTables' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Timestream_20181101.ListTables" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTables where
  toJSON ListTables' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListTables where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
  { -- | A list of tables.
    tables :: Prelude.Maybe [Table],
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tables', 'listTablesResponse_tables' - A list of tables.
--
-- 'nextToken', 'listTablesResponse_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'httpStatus', 'listTablesResponse_httpStatus' - The response's http status code.
newListTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTablesResponse
newListTablesResponse pHttpStatus_ =
  ListTablesResponse'
    { tables = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tables.
listTablesResponse_tables :: Lens.Lens' ListTablesResponse (Prelude.Maybe [Table])
listTablesResponse_tables = Lens.lens (\ListTablesResponse' {tables} -> tables) (\s@ListTablesResponse' {} a -> s {tables = a} :: ListTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listTablesResponse_nextToken :: Lens.Lens' ListTablesResponse (Prelude.Maybe Prelude.Text)
listTablesResponse_nextToken = Lens.lens (\ListTablesResponse' {nextToken} -> nextToken) (\s@ListTablesResponse' {} a -> s {nextToken = a} :: ListTablesResponse)

-- | The response's http status code.
listTablesResponse_httpStatus :: Lens.Lens' ListTablesResponse Prelude.Int
listTablesResponse_httpStatus = Lens.lens (\ListTablesResponse' {httpStatus} -> httpStatus) (\s@ListTablesResponse' {} a -> s {httpStatus = a} :: ListTablesResponse)

instance Prelude.NFData ListTablesResponse where
  rnf ListTablesResponse' {..} =
    Prelude.rnf tables
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
