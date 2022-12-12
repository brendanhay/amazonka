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
-- Module      : Amazonka.QLDB.ListJournalS3ExportsForLedger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of journal export job descriptions for a specified
-- ledger.
--
-- This action returns a maximum of @MaxResults@ items, and is paginated so
-- that you can retrieve all the items by calling
-- @ListJournalS3ExportsForLedger@ multiple times.
--
-- This action does not return any expired export jobs. For more
-- information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/export-journal.request.html#export-journal.request.expiration Export job expiration>
-- in the /Amazon QLDB Developer Guide/.
module Amazonka.QLDB.ListJournalS3ExportsForLedger
  ( -- * Creating a Request
    ListJournalS3ExportsForLedger (..),
    newListJournalS3ExportsForLedger,

    -- * Request Lenses
    listJournalS3ExportsForLedger_maxResults,
    listJournalS3ExportsForLedger_nextToken,
    listJournalS3ExportsForLedger_name,

    -- * Destructuring the Response
    ListJournalS3ExportsForLedgerResponse (..),
    newListJournalS3ExportsForLedgerResponse,

    -- * Response Lenses
    listJournalS3ExportsForLedgerResponse_journalS3Exports,
    listJournalS3ExportsForLedgerResponse_nextToken,
    listJournalS3ExportsForLedgerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJournalS3ExportsForLedger' smart constructor.
data ListJournalS3ExportsForLedger = ListJournalS3ExportsForLedger'
  { -- | The maximum number of results to return in a single
    -- @ListJournalS3ExportsForLedger@ request. (The actual number of results
    -- returned might be fewer.)
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token, indicating that you want to retrieve the next page
    -- of results. If you received a value for @NextToken@ in the response from
    -- a previous @ListJournalS3ExportsForLedger@ call, then you should use
    -- that value as input here.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the ledger.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJournalS3ExportsForLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listJournalS3ExportsForLedger_maxResults' - The maximum number of results to return in a single
-- @ListJournalS3ExportsForLedger@ request. (The actual number of results
-- returned might be fewer.)
--
-- 'nextToken', 'listJournalS3ExportsForLedger_nextToken' - A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalS3ExportsForLedger@ call, then you should use
-- that value as input here.
--
-- 'name', 'listJournalS3ExportsForLedger_name' - The name of the ledger.
newListJournalS3ExportsForLedger ::
  -- | 'name'
  Prelude.Text ->
  ListJournalS3ExportsForLedger
newListJournalS3ExportsForLedger pName_ =
  ListJournalS3ExportsForLedger'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      name = pName_
    }

-- | The maximum number of results to return in a single
-- @ListJournalS3ExportsForLedger@ request. (The actual number of results
-- returned might be fewer.)
listJournalS3ExportsForLedger_maxResults :: Lens.Lens' ListJournalS3ExportsForLedger (Prelude.Maybe Prelude.Natural)
listJournalS3ExportsForLedger_maxResults = Lens.lens (\ListJournalS3ExportsForLedger' {maxResults} -> maxResults) (\s@ListJournalS3ExportsForLedger' {} a -> s {maxResults = a} :: ListJournalS3ExportsForLedger)

-- | A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalS3ExportsForLedger@ call, then you should use
-- that value as input here.
listJournalS3ExportsForLedger_nextToken :: Lens.Lens' ListJournalS3ExportsForLedger (Prelude.Maybe Prelude.Text)
listJournalS3ExportsForLedger_nextToken = Lens.lens (\ListJournalS3ExportsForLedger' {nextToken} -> nextToken) (\s@ListJournalS3ExportsForLedger' {} a -> s {nextToken = a} :: ListJournalS3ExportsForLedger)

-- | The name of the ledger.
listJournalS3ExportsForLedger_name :: Lens.Lens' ListJournalS3ExportsForLedger Prelude.Text
listJournalS3ExportsForLedger_name = Lens.lens (\ListJournalS3ExportsForLedger' {name} -> name) (\s@ListJournalS3ExportsForLedger' {} a -> s {name = a} :: ListJournalS3ExportsForLedger)

instance
  Core.AWSRequest
    ListJournalS3ExportsForLedger
  where
  type
    AWSResponse ListJournalS3ExportsForLedger =
      ListJournalS3ExportsForLedgerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJournalS3ExportsForLedgerResponse'
            Prelude.<$> ( x Data..?> "JournalS3Exports"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListJournalS3ExportsForLedger
  where
  hashWithSalt _salt ListJournalS3ExportsForLedger' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListJournalS3ExportsForLedger where
  rnf ListJournalS3ExportsForLedger' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders ListJournalS3ExportsForLedger where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListJournalS3ExportsForLedger where
  toPath ListJournalS3ExportsForLedger' {..} =
    Prelude.mconcat
      ["/ledgers/", Data.toBS name, "/journal-s3-exports"]

instance Data.ToQuery ListJournalS3ExportsForLedger where
  toQuery ListJournalS3ExportsForLedger' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "next_token" Data.=: nextToken
      ]

-- | /See:/ 'newListJournalS3ExportsForLedgerResponse' smart constructor.
data ListJournalS3ExportsForLedgerResponse = ListJournalS3ExportsForLedgerResponse'
  { -- | The array of journal export job descriptions that are associated with
    -- the specified ledger.
    journalS3Exports :: Prelude.Maybe [JournalS3ExportDescription],
    -- | -   If @NextToken@ is empty, then the last page of results has been
    --     processed and there are no more results to be retrieved.
    --
    -- -   If @NextToken@ is /not/ empty, then there are more results
    --     available. To retrieve the next page of results, use the value of
    --     @NextToken@ in a subsequent @ListJournalS3ExportsForLedger@ call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJournalS3ExportsForLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'journalS3Exports', 'listJournalS3ExportsForLedgerResponse_journalS3Exports' - The array of journal export job descriptions that are associated with
-- the specified ledger.
--
-- 'nextToken', 'listJournalS3ExportsForLedgerResponse_nextToken' - -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListJournalS3ExportsForLedger@ call.
--
-- 'httpStatus', 'listJournalS3ExportsForLedgerResponse_httpStatus' - The response's http status code.
newListJournalS3ExportsForLedgerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJournalS3ExportsForLedgerResponse
newListJournalS3ExportsForLedgerResponse pHttpStatus_ =
  ListJournalS3ExportsForLedgerResponse'
    { journalS3Exports =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of journal export job descriptions that are associated with
-- the specified ledger.
listJournalS3ExportsForLedgerResponse_journalS3Exports :: Lens.Lens' ListJournalS3ExportsForLedgerResponse (Prelude.Maybe [JournalS3ExportDescription])
listJournalS3ExportsForLedgerResponse_journalS3Exports = Lens.lens (\ListJournalS3ExportsForLedgerResponse' {journalS3Exports} -> journalS3Exports) (\s@ListJournalS3ExportsForLedgerResponse' {} a -> s {journalS3Exports = a} :: ListJournalS3ExportsForLedgerResponse) Prelude.. Lens.mapping Lens.coerced

-- | -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListJournalS3ExportsForLedger@ call.
listJournalS3ExportsForLedgerResponse_nextToken :: Lens.Lens' ListJournalS3ExportsForLedgerResponse (Prelude.Maybe Prelude.Text)
listJournalS3ExportsForLedgerResponse_nextToken = Lens.lens (\ListJournalS3ExportsForLedgerResponse' {nextToken} -> nextToken) (\s@ListJournalS3ExportsForLedgerResponse' {} a -> s {nextToken = a} :: ListJournalS3ExportsForLedgerResponse)

-- | The response's http status code.
listJournalS3ExportsForLedgerResponse_httpStatus :: Lens.Lens' ListJournalS3ExportsForLedgerResponse Prelude.Int
listJournalS3ExportsForLedgerResponse_httpStatus = Lens.lens (\ListJournalS3ExportsForLedgerResponse' {httpStatus} -> httpStatus) (\s@ListJournalS3ExportsForLedgerResponse' {} a -> s {httpStatus = a} :: ListJournalS3ExportsForLedgerResponse)

instance
  Prelude.NFData
    ListJournalS3ExportsForLedgerResponse
  where
  rnf ListJournalS3ExportsForLedgerResponse' {..} =
    Prelude.rnf journalS3Exports
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
