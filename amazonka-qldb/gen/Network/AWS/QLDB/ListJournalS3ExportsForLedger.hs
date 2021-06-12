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
-- Module      : Network.AWS.QLDB.ListJournalS3ExportsForLedger
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/export-journal.request.html#export-journal.request.expiration Export Job Expiration>
-- in the /Amazon QLDB Developer Guide/.
module Network.AWS.QLDB.ListJournalS3ExportsForLedger
  ( -- * Creating a Request
    ListJournalS3ExportsForLedger (..),
    newListJournalS3ExportsForLedger,

    -- * Request Lenses
    listJournalS3ExportsForLedger_nextToken,
    listJournalS3ExportsForLedger_maxResults,
    listJournalS3ExportsForLedger_name,

    -- * Destructuring the Response
    ListJournalS3ExportsForLedgerResponse (..),
    newListJournalS3ExportsForLedgerResponse,

    -- * Response Lenses
    listJournalS3ExportsForLedgerResponse_nextToken,
    listJournalS3ExportsForLedgerResponse_journalS3Exports,
    listJournalS3ExportsForLedgerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListJournalS3ExportsForLedger' smart constructor.
data ListJournalS3ExportsForLedger = ListJournalS3ExportsForLedger'
  { -- | A pagination token, indicating that you want to retrieve the next page
    -- of results. If you received a value for @NextToken@ in the response from
    -- a previous @ListJournalS3ExportsForLedger@ call, then you should use
    -- that value as input here.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single
    -- @ListJournalS3ExportsForLedger@ request. (The actual number of results
    -- returned might be fewer.)
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the ledger.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJournalS3ExportsForLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJournalS3ExportsForLedger_nextToken' - A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalS3ExportsForLedger@ call, then you should use
-- that value as input here.
--
-- 'maxResults', 'listJournalS3ExportsForLedger_maxResults' - The maximum number of results to return in a single
-- @ListJournalS3ExportsForLedger@ request. (The actual number of results
-- returned might be fewer.)
--
-- 'name', 'listJournalS3ExportsForLedger_name' - The name of the ledger.
newListJournalS3ExportsForLedger ::
  -- | 'name'
  Core.Text ->
  ListJournalS3ExportsForLedger
newListJournalS3ExportsForLedger pName_ =
  ListJournalS3ExportsForLedger'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      name = pName_
    }

-- | A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalS3ExportsForLedger@ call, then you should use
-- that value as input here.
listJournalS3ExportsForLedger_nextToken :: Lens.Lens' ListJournalS3ExportsForLedger (Core.Maybe Core.Text)
listJournalS3ExportsForLedger_nextToken = Lens.lens (\ListJournalS3ExportsForLedger' {nextToken} -> nextToken) (\s@ListJournalS3ExportsForLedger' {} a -> s {nextToken = a} :: ListJournalS3ExportsForLedger)

-- | The maximum number of results to return in a single
-- @ListJournalS3ExportsForLedger@ request. (The actual number of results
-- returned might be fewer.)
listJournalS3ExportsForLedger_maxResults :: Lens.Lens' ListJournalS3ExportsForLedger (Core.Maybe Core.Natural)
listJournalS3ExportsForLedger_maxResults = Lens.lens (\ListJournalS3ExportsForLedger' {maxResults} -> maxResults) (\s@ListJournalS3ExportsForLedger' {} a -> s {maxResults = a} :: ListJournalS3ExportsForLedger)

-- | The name of the ledger.
listJournalS3ExportsForLedger_name :: Lens.Lens' ListJournalS3ExportsForLedger Core.Text
listJournalS3ExportsForLedger_name = Lens.lens (\ListJournalS3ExportsForLedger' {name} -> name) (\s@ListJournalS3ExportsForLedger' {} a -> s {name = a} :: ListJournalS3ExportsForLedger)

instance
  Core.AWSRequest
    ListJournalS3ExportsForLedger
  where
  type
    AWSResponse ListJournalS3ExportsForLedger =
      ListJournalS3ExportsForLedgerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJournalS3ExportsForLedgerResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "JournalS3Exports" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJournalS3ExportsForLedger

instance Core.NFData ListJournalS3ExportsForLedger

instance Core.ToHeaders ListJournalS3ExportsForLedger where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListJournalS3ExportsForLedger where
  toPath ListJournalS3ExportsForLedger' {..} =
    Core.mconcat
      ["/ledgers/", Core.toBS name, "/journal-s3-exports"]

instance Core.ToQuery ListJournalS3ExportsForLedger where
  toQuery ListJournalS3ExportsForLedger' {..} =
    Core.mconcat
      [ "next_token" Core.=: nextToken,
        "max_results" Core.=: maxResults
      ]

-- | /See:/ 'newListJournalS3ExportsForLedgerResponse' smart constructor.
data ListJournalS3ExportsForLedgerResponse = ListJournalS3ExportsForLedgerResponse'
  { -- | -   If @NextToken@ is empty, then the last page of results has been
    --     processed and there are no more results to be retrieved.
    --
    -- -   If @NextToken@ is /not/ empty, then there are more results
    --     available. To retrieve the next page of results, use the value of
    --     @NextToken@ in a subsequent @ListJournalS3ExportsForLedger@ call.
    nextToken :: Core.Maybe Core.Text,
    -- | The array of journal export job descriptions that are associated with
    -- the specified ledger.
    journalS3Exports :: Core.Maybe [JournalS3ExportDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJournalS3ExportsForLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJournalS3ExportsForLedgerResponse_nextToken' - -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListJournalS3ExportsForLedger@ call.
--
-- 'journalS3Exports', 'listJournalS3ExportsForLedgerResponse_journalS3Exports' - The array of journal export job descriptions that are associated with
-- the specified ledger.
--
-- 'httpStatus', 'listJournalS3ExportsForLedgerResponse_httpStatus' - The response's http status code.
newListJournalS3ExportsForLedgerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJournalS3ExportsForLedgerResponse
newListJournalS3ExportsForLedgerResponse pHttpStatus_ =
  ListJournalS3ExportsForLedgerResponse'
    { nextToken =
        Core.Nothing,
      journalS3Exports = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListJournalS3ExportsForLedger@ call.
listJournalS3ExportsForLedgerResponse_nextToken :: Lens.Lens' ListJournalS3ExportsForLedgerResponse (Core.Maybe Core.Text)
listJournalS3ExportsForLedgerResponse_nextToken = Lens.lens (\ListJournalS3ExportsForLedgerResponse' {nextToken} -> nextToken) (\s@ListJournalS3ExportsForLedgerResponse' {} a -> s {nextToken = a} :: ListJournalS3ExportsForLedgerResponse)

-- | The array of journal export job descriptions that are associated with
-- the specified ledger.
listJournalS3ExportsForLedgerResponse_journalS3Exports :: Lens.Lens' ListJournalS3ExportsForLedgerResponse (Core.Maybe [JournalS3ExportDescription])
listJournalS3ExportsForLedgerResponse_journalS3Exports = Lens.lens (\ListJournalS3ExportsForLedgerResponse' {journalS3Exports} -> journalS3Exports) (\s@ListJournalS3ExportsForLedgerResponse' {} a -> s {journalS3Exports = a} :: ListJournalS3ExportsForLedgerResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJournalS3ExportsForLedgerResponse_httpStatus :: Lens.Lens' ListJournalS3ExportsForLedgerResponse Core.Int
listJournalS3ExportsForLedgerResponse_httpStatus = Lens.lens (\ListJournalS3ExportsForLedgerResponse' {httpStatus} -> httpStatus) (\s@ListJournalS3ExportsForLedgerResponse' {} a -> s {httpStatus = a} :: ListJournalS3ExportsForLedgerResponse)

instance
  Core.NFData
    ListJournalS3ExportsForLedgerResponse
