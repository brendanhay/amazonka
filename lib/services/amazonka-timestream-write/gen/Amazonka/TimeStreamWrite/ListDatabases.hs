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
-- Module      : Amazonka.TimeStreamWrite.ListDatabases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of your Timestream databases.
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html Service quotas apply>.
-- See
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.list-db.html code sample>
-- for details.
module Amazonka.TimeStreamWrite.ListDatabases
  ( -- * Creating a Request
    ListDatabases (..),
    newListDatabases,

    -- * Request Lenses
    listDatabases_maxResults,
    listDatabases_nextToken,

    -- * Destructuring the Response
    ListDatabasesResponse (..),
    newListDatabasesResponse,

    -- * Response Lenses
    listDatabasesResponse_databases,
    listDatabasesResponse_nextToken,
    listDatabasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamWrite.Types

-- | /See:/ 'newListDatabases' smart constructor.
data ListDatabases = ListDatabases'
  { -- | The total number of items to return in the output. If the total number
    -- of items available is more than the value specified, a NextToken is
    -- provided in the output. To resume pagination, provide the NextToken
    -- value as argument of a subsequent API invocation.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token. To resume pagination, provide the NextToken value
    -- as argument of a subsequent API invocation.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatabases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDatabases_maxResults' - The total number of items to return in the output. If the total number
-- of items available is more than the value specified, a NextToken is
-- provided in the output. To resume pagination, provide the NextToken
-- value as argument of a subsequent API invocation.
--
-- 'nextToken', 'listDatabases_nextToken' - The pagination token. To resume pagination, provide the NextToken value
-- as argument of a subsequent API invocation.
newListDatabases ::
  ListDatabases
newListDatabases =
  ListDatabases'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The total number of items to return in the output. If the total number
-- of items available is more than the value specified, a NextToken is
-- provided in the output. To resume pagination, provide the NextToken
-- value as argument of a subsequent API invocation.
listDatabases_maxResults :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Natural)
listDatabases_maxResults = Lens.lens (\ListDatabases' {maxResults} -> maxResults) (\s@ListDatabases' {} a -> s {maxResults = a} :: ListDatabases)

-- | The pagination token. To resume pagination, provide the NextToken value
-- as argument of a subsequent API invocation.
listDatabases_nextToken :: Lens.Lens' ListDatabases (Prelude.Maybe Prelude.Text)
listDatabases_nextToken = Lens.lens (\ListDatabases' {nextToken} -> nextToken) (\s@ListDatabases' {} a -> s {nextToken = a} :: ListDatabases)

instance Core.AWSRequest ListDatabases where
  type
    AWSResponse ListDatabases =
      ListDatabasesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatabasesResponse'
            Prelude.<$> (x Data..?> "Databases" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatabases where
  hashWithSalt _salt ListDatabases' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDatabases where
  rnf ListDatabases' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDatabases where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.ListDatabases" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDatabases where
  toJSON ListDatabases' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDatabases where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDatabases where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDatabasesResponse' smart constructor.
data ListDatabasesResponse = ListDatabasesResponse'
  { -- | A list of database names.
    databases :: Prelude.Maybe [Database],
    -- | The pagination token. This parameter is returned when the response is
    -- truncated.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatabasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databases', 'listDatabasesResponse_databases' - A list of database names.
--
-- 'nextToken', 'listDatabasesResponse_nextToken' - The pagination token. This parameter is returned when the response is
-- truncated.
--
-- 'httpStatus', 'listDatabasesResponse_httpStatus' - The response's http status code.
newListDatabasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatabasesResponse
newListDatabasesResponse pHttpStatus_ =
  ListDatabasesResponse'
    { databases = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of database names.
listDatabasesResponse_databases :: Lens.Lens' ListDatabasesResponse (Prelude.Maybe [Database])
listDatabasesResponse_databases = Lens.lens (\ListDatabasesResponse' {databases} -> databases) (\s@ListDatabasesResponse' {} a -> s {databases = a} :: ListDatabasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token. This parameter is returned when the response is
-- truncated.
listDatabasesResponse_nextToken :: Lens.Lens' ListDatabasesResponse (Prelude.Maybe Prelude.Text)
listDatabasesResponse_nextToken = Lens.lens (\ListDatabasesResponse' {nextToken} -> nextToken) (\s@ListDatabasesResponse' {} a -> s {nextToken = a} :: ListDatabasesResponse)

-- | The response's http status code.
listDatabasesResponse_httpStatus :: Lens.Lens' ListDatabasesResponse Prelude.Int
listDatabasesResponse_httpStatus = Lens.lens (\ListDatabasesResponse' {httpStatus} -> httpStatus) (\s@ListDatabasesResponse' {} a -> s {httpStatus = a} :: ListDatabasesResponse)

instance Prelude.NFData ListDatabasesResponse where
  rnf ListDatabasesResponse' {..} =
    Prelude.rnf databases
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
