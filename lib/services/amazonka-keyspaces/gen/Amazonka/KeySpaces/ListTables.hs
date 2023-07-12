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
-- Module      : Amazonka.KeySpaces.ListTables
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of tables for a specified keyspace.
--
-- This operation returns paginated results.
module Amazonka.KeySpaces.ListTables
  ( -- * Creating a Request
    ListTables (..),
    newListTables,

    -- * Request Lenses
    listTables_maxResults,
    listTables_nextToken,
    listTables_keyspaceName,

    -- * Destructuring the Response
    ListTablesResponse (..),
    newListTablesResponse,

    -- * Response Lenses
    listTablesResponse_nextToken,
    listTablesResponse_tables,
    listTablesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTables' smart constructor.
data ListTables = ListTables'
  { -- | The total number of tables to return in the output. If the total number
    -- of tables available is more than the value specified, a @NextToken@ is
    -- provided in the output. To resume pagination, provide the @NextToken@
    -- value as an argument of a subsequent API invocation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token. To resume pagination, provide the @NextToken@
    -- value as an argument of a subsequent API invocation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the keyspace.
    keyspaceName :: Prelude.Text
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
-- 'maxResults', 'listTables_maxResults' - The total number of tables to return in the output. If the total number
-- of tables available is more than the value specified, a @NextToken@ is
-- provided in the output. To resume pagination, provide the @NextToken@
-- value as an argument of a subsequent API invocation.
--
-- 'nextToken', 'listTables_nextToken' - The pagination token. To resume pagination, provide the @NextToken@
-- value as an argument of a subsequent API invocation.
--
-- 'keyspaceName', 'listTables_keyspaceName' - The name of the keyspace.
newListTables ::
  -- | 'keyspaceName'
  Prelude.Text ->
  ListTables
newListTables pKeyspaceName_ =
  ListTables'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      keyspaceName = pKeyspaceName_
    }

-- | The total number of tables to return in the output. If the total number
-- of tables available is more than the value specified, a @NextToken@ is
-- provided in the output. To resume pagination, provide the @NextToken@
-- value as an argument of a subsequent API invocation.
listTables_maxResults :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Natural)
listTables_maxResults = Lens.lens (\ListTables' {maxResults} -> maxResults) (\s@ListTables' {} a -> s {maxResults = a} :: ListTables)

-- | The pagination token. To resume pagination, provide the @NextToken@
-- value as an argument of a subsequent API invocation.
listTables_nextToken :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_nextToken = Lens.lens (\ListTables' {nextToken} -> nextToken) (\s@ListTables' {} a -> s {nextToken = a} :: ListTables)

-- | The name of the keyspace.
listTables_keyspaceName :: Lens.Lens' ListTables Prelude.Text
listTables_keyspaceName = Lens.lens (\ListTables' {keyspaceName} -> keyspaceName) (\s@ListTables' {} a -> s {keyspaceName = a} :: ListTables)

instance Core.AWSPager ListTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_tables
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTables_nextToken
          Lens..~ rs
          Lens.^? listTablesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTables where
  type AWSResponse ListTables = ListTablesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTablesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "tables" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTables where
  hashWithSalt _salt ListTables' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` keyspaceName

instance Prelude.NFData ListTables where
  rnf ListTables' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf keyspaceName

instance Data.ToHeaders ListTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KeyspacesService.ListTables" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTables where
  toJSON ListTables' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("keyspaceName" Data..= keyspaceName)
          ]
      )

instance Data.ToPath ListTables where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTables where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
  { -- | A token to specify where to start paginating. This is the @NextToken@
    -- from a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of tables.
    tables :: Prelude.Maybe [TableSummary],
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
-- 'nextToken', 'listTablesResponse_nextToken' - A token to specify where to start paginating. This is the @NextToken@
-- from a previously truncated response.
--
-- 'tables', 'listTablesResponse_tables' - A list of tables.
--
-- 'httpStatus', 'listTablesResponse_httpStatus' - The response's http status code.
newListTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTablesResponse
newListTablesResponse pHttpStatus_ =
  ListTablesResponse'
    { nextToken = Prelude.Nothing,
      tables = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token to specify where to start paginating. This is the @NextToken@
-- from a previously truncated response.
listTablesResponse_nextToken :: Lens.Lens' ListTablesResponse (Prelude.Maybe Prelude.Text)
listTablesResponse_nextToken = Lens.lens (\ListTablesResponse' {nextToken} -> nextToken) (\s@ListTablesResponse' {} a -> s {nextToken = a} :: ListTablesResponse)

-- | A list of tables.
listTablesResponse_tables :: Lens.Lens' ListTablesResponse (Prelude.Maybe [TableSummary])
listTablesResponse_tables = Lens.lens (\ListTablesResponse' {tables} -> tables) (\s@ListTablesResponse' {} a -> s {tables = a} :: ListTablesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTablesResponse_httpStatus :: Lens.Lens' ListTablesResponse Prelude.Int
listTablesResponse_httpStatus = Lens.lens (\ListTablesResponse' {httpStatus} -> httpStatus) (\s@ListTablesResponse' {} a -> s {httpStatus = a} :: ListTablesResponse)

instance Prelude.NFData ListTablesResponse where
  rnf ListTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tables
      `Prelude.seq` Prelude.rnf httpStatus
