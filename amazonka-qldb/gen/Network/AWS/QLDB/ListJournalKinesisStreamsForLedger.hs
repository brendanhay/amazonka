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
-- Module      : Network.AWS.QLDB.ListJournalKinesisStreamsForLedger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of all Amazon QLDB journal stream descriptors for a
-- given ledger. The output of each stream descriptor includes the same
-- details that are returned by @DescribeJournalKinesisStream@.
--
-- This action returns a maximum of @MaxResults@ items. It is paginated so
-- that you can retrieve all the items by calling
-- @ListJournalKinesisStreamsForLedger@ multiple times.
module Network.AWS.QLDB.ListJournalKinesisStreamsForLedger
  ( -- * Creating a Request
    ListJournalKinesisStreamsForLedger (..),
    newListJournalKinesisStreamsForLedger,

    -- * Request Lenses
    listJournalKinesisStreamsForLedger_nextToken,
    listJournalKinesisStreamsForLedger_maxResults,
    listJournalKinesisStreamsForLedger_ledgerName,

    -- * Destructuring the Response
    ListJournalKinesisStreamsForLedgerResponse (..),
    newListJournalKinesisStreamsForLedgerResponse,

    -- * Response Lenses
    listJournalKinesisStreamsForLedgerResponse_streams,
    listJournalKinesisStreamsForLedgerResponse_nextToken,
    listJournalKinesisStreamsForLedgerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListJournalKinesisStreamsForLedger' smart constructor.
data ListJournalKinesisStreamsForLedger = ListJournalKinesisStreamsForLedger'
  { -- | A pagination token, indicating that you want to retrieve the next page
    -- of results. If you received a value for @NextToken@ in the response from
    -- a previous @ListJournalKinesisStreamsForLedger@ call, you should use
    -- that value as input here.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a single
    -- @ListJournalKinesisStreamsForLedger@ request. (The actual number of
    -- results returned might be fewer.)
    maxResults :: Core.Maybe Core.Natural,
    -- | The name of the ledger.
    ledgerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJournalKinesisStreamsForLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJournalKinesisStreamsForLedger_nextToken' - A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalKinesisStreamsForLedger@ call, you should use
-- that value as input here.
--
-- 'maxResults', 'listJournalKinesisStreamsForLedger_maxResults' - The maximum number of results to return in a single
-- @ListJournalKinesisStreamsForLedger@ request. (The actual number of
-- results returned might be fewer.)
--
-- 'ledgerName', 'listJournalKinesisStreamsForLedger_ledgerName' - The name of the ledger.
newListJournalKinesisStreamsForLedger ::
  -- | 'ledgerName'
  Core.Text ->
  ListJournalKinesisStreamsForLedger
newListJournalKinesisStreamsForLedger pLedgerName_ =
  ListJournalKinesisStreamsForLedger'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      ledgerName = pLedgerName_
    }

-- | A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalKinesisStreamsForLedger@ call, you should use
-- that value as input here.
listJournalKinesisStreamsForLedger_nextToken :: Lens.Lens' ListJournalKinesisStreamsForLedger (Core.Maybe Core.Text)
listJournalKinesisStreamsForLedger_nextToken = Lens.lens (\ListJournalKinesisStreamsForLedger' {nextToken} -> nextToken) (\s@ListJournalKinesisStreamsForLedger' {} a -> s {nextToken = a} :: ListJournalKinesisStreamsForLedger)

-- | The maximum number of results to return in a single
-- @ListJournalKinesisStreamsForLedger@ request. (The actual number of
-- results returned might be fewer.)
listJournalKinesisStreamsForLedger_maxResults :: Lens.Lens' ListJournalKinesisStreamsForLedger (Core.Maybe Core.Natural)
listJournalKinesisStreamsForLedger_maxResults = Lens.lens (\ListJournalKinesisStreamsForLedger' {maxResults} -> maxResults) (\s@ListJournalKinesisStreamsForLedger' {} a -> s {maxResults = a} :: ListJournalKinesisStreamsForLedger)

-- | The name of the ledger.
listJournalKinesisStreamsForLedger_ledgerName :: Lens.Lens' ListJournalKinesisStreamsForLedger Core.Text
listJournalKinesisStreamsForLedger_ledgerName = Lens.lens (\ListJournalKinesisStreamsForLedger' {ledgerName} -> ledgerName) (\s@ListJournalKinesisStreamsForLedger' {} a -> s {ledgerName = a} :: ListJournalKinesisStreamsForLedger)

instance
  Core.AWSRequest
    ListJournalKinesisStreamsForLedger
  where
  type
    AWSResponse ListJournalKinesisStreamsForLedger =
      ListJournalKinesisStreamsForLedgerResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJournalKinesisStreamsForLedgerResponse'
            Core.<$> (x Core..?> "Streams" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListJournalKinesisStreamsForLedger

instance
  Core.NFData
    ListJournalKinesisStreamsForLedger

instance
  Core.ToHeaders
    ListJournalKinesisStreamsForLedger
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance
  Core.ToPath
    ListJournalKinesisStreamsForLedger
  where
  toPath ListJournalKinesisStreamsForLedger' {..} =
    Core.mconcat
      [ "/ledgers/",
        Core.toBS ledgerName,
        "/journal-kinesis-streams"
      ]

instance
  Core.ToQuery
    ListJournalKinesisStreamsForLedger
  where
  toQuery ListJournalKinesisStreamsForLedger' {..} =
    Core.mconcat
      [ "next_token" Core.=: nextToken,
        "max_results" Core.=: maxResults
      ]

-- | /See:/ 'newListJournalKinesisStreamsForLedgerResponse' smart constructor.
data ListJournalKinesisStreamsForLedgerResponse = ListJournalKinesisStreamsForLedgerResponse'
  { -- | The array of QLDB journal stream descriptors that are associated with
    -- the given ledger.
    streams :: Core.Maybe [JournalKinesisStreamDescription],
    -- | -   If @NextToken@ is empty, the last page of results has been processed
    --     and there are no more results to be retrieved.
    --
    -- -   If @NextToken@ is /not/ empty, more results are available. To
    --     retrieve the next page of results, use the value of @NextToken@ in a
    --     subsequent @ListJournalKinesisStreamsForLedger@ call.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJournalKinesisStreamsForLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streams', 'listJournalKinesisStreamsForLedgerResponse_streams' - The array of QLDB journal stream descriptors that are associated with
-- the given ledger.
--
-- 'nextToken', 'listJournalKinesisStreamsForLedgerResponse_nextToken' - -   If @NextToken@ is empty, the last page of results has been processed
--     and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, more results are available. To
--     retrieve the next page of results, use the value of @NextToken@ in a
--     subsequent @ListJournalKinesisStreamsForLedger@ call.
--
-- 'httpStatus', 'listJournalKinesisStreamsForLedgerResponse_httpStatus' - The response's http status code.
newListJournalKinesisStreamsForLedgerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJournalKinesisStreamsForLedgerResponse
newListJournalKinesisStreamsForLedgerResponse
  pHttpStatus_ =
    ListJournalKinesisStreamsForLedgerResponse'
      { streams =
          Core.Nothing,
        nextToken = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The array of QLDB journal stream descriptors that are associated with
-- the given ledger.
listJournalKinesisStreamsForLedgerResponse_streams :: Lens.Lens' ListJournalKinesisStreamsForLedgerResponse (Core.Maybe [JournalKinesisStreamDescription])
listJournalKinesisStreamsForLedgerResponse_streams = Lens.lens (\ListJournalKinesisStreamsForLedgerResponse' {streams} -> streams) (\s@ListJournalKinesisStreamsForLedgerResponse' {} a -> s {streams = a} :: ListJournalKinesisStreamsForLedgerResponse) Core.. Lens.mapping Lens._Coerce

-- | -   If @NextToken@ is empty, the last page of results has been processed
--     and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, more results are available. To
--     retrieve the next page of results, use the value of @NextToken@ in a
--     subsequent @ListJournalKinesisStreamsForLedger@ call.
listJournalKinesisStreamsForLedgerResponse_nextToken :: Lens.Lens' ListJournalKinesisStreamsForLedgerResponse (Core.Maybe Core.Text)
listJournalKinesisStreamsForLedgerResponse_nextToken = Lens.lens (\ListJournalKinesisStreamsForLedgerResponse' {nextToken} -> nextToken) (\s@ListJournalKinesisStreamsForLedgerResponse' {} a -> s {nextToken = a} :: ListJournalKinesisStreamsForLedgerResponse)

-- | The response's http status code.
listJournalKinesisStreamsForLedgerResponse_httpStatus :: Lens.Lens' ListJournalKinesisStreamsForLedgerResponse Core.Int
listJournalKinesisStreamsForLedgerResponse_httpStatus = Lens.lens (\ListJournalKinesisStreamsForLedgerResponse' {httpStatus} -> httpStatus) (\s@ListJournalKinesisStreamsForLedgerResponse' {} a -> s {httpStatus = a} :: ListJournalKinesisStreamsForLedgerResponse)

instance
  Core.NFData
    ListJournalKinesisStreamsForLedgerResponse
