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
-- Module      : Amazonka.QLDB.ListJournalKinesisStreamsForLedger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of all Amazon QLDB journal stream descriptors for a
-- given ledger. The output of each stream descriptor includes the same
-- details that are returned by @DescribeJournalKinesisStream@.
--
-- This action does not return any expired journal streams. For more
-- information, see
-- <https://docs.aws.amazon.com/qldb/latest/developerguide/streams.create.html#streams.create.states.expiration Expiration for terminal streams>
-- in the /Amazon QLDB Developer Guide/.
--
-- This action returns a maximum of @MaxResults@ items. It is paginated so
-- that you can retrieve all the items by calling
-- @ListJournalKinesisStreamsForLedger@ multiple times.
module Amazonka.QLDB.ListJournalKinesisStreamsForLedger
  ( -- * Creating a Request
    ListJournalKinesisStreamsForLedger (..),
    newListJournalKinesisStreamsForLedger,

    -- * Request Lenses
    listJournalKinesisStreamsForLedger_maxResults,
    listJournalKinesisStreamsForLedger_nextToken,
    listJournalKinesisStreamsForLedger_ledgerName,

    -- * Destructuring the Response
    ListJournalKinesisStreamsForLedgerResponse (..),
    newListJournalKinesisStreamsForLedgerResponse,

    -- * Response Lenses
    listJournalKinesisStreamsForLedgerResponse_nextToken,
    listJournalKinesisStreamsForLedgerResponse_streams,
    listJournalKinesisStreamsForLedgerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJournalKinesisStreamsForLedger' smart constructor.
data ListJournalKinesisStreamsForLedger = ListJournalKinesisStreamsForLedger'
  { -- | The maximum number of results to return in a single
    -- @ListJournalKinesisStreamsForLedger@ request. (The actual number of
    -- results returned might be fewer.)
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token, indicating that you want to retrieve the next page
    -- of results. If you received a value for @NextToken@ in the response from
    -- a previous @ListJournalKinesisStreamsForLedger@ call, you should use
    -- that value as input here.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the ledger.
    ledgerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJournalKinesisStreamsForLedger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listJournalKinesisStreamsForLedger_maxResults' - The maximum number of results to return in a single
-- @ListJournalKinesisStreamsForLedger@ request. (The actual number of
-- results returned might be fewer.)
--
-- 'nextToken', 'listJournalKinesisStreamsForLedger_nextToken' - A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalKinesisStreamsForLedger@ call, you should use
-- that value as input here.
--
-- 'ledgerName', 'listJournalKinesisStreamsForLedger_ledgerName' - The name of the ledger.
newListJournalKinesisStreamsForLedger ::
  -- | 'ledgerName'
  Prelude.Text ->
  ListJournalKinesisStreamsForLedger
newListJournalKinesisStreamsForLedger pLedgerName_ =
  ListJournalKinesisStreamsForLedger'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      ledgerName = pLedgerName_
    }

-- | The maximum number of results to return in a single
-- @ListJournalKinesisStreamsForLedger@ request. (The actual number of
-- results returned might be fewer.)
listJournalKinesisStreamsForLedger_maxResults :: Lens.Lens' ListJournalKinesisStreamsForLedger (Prelude.Maybe Prelude.Natural)
listJournalKinesisStreamsForLedger_maxResults = Lens.lens (\ListJournalKinesisStreamsForLedger' {maxResults} -> maxResults) (\s@ListJournalKinesisStreamsForLedger' {} a -> s {maxResults = a} :: ListJournalKinesisStreamsForLedger)

-- | A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListJournalKinesisStreamsForLedger@ call, you should use
-- that value as input here.
listJournalKinesisStreamsForLedger_nextToken :: Lens.Lens' ListJournalKinesisStreamsForLedger (Prelude.Maybe Prelude.Text)
listJournalKinesisStreamsForLedger_nextToken = Lens.lens (\ListJournalKinesisStreamsForLedger' {nextToken} -> nextToken) (\s@ListJournalKinesisStreamsForLedger' {} a -> s {nextToken = a} :: ListJournalKinesisStreamsForLedger)

-- | The name of the ledger.
listJournalKinesisStreamsForLedger_ledgerName :: Lens.Lens' ListJournalKinesisStreamsForLedger Prelude.Text
listJournalKinesisStreamsForLedger_ledgerName = Lens.lens (\ListJournalKinesisStreamsForLedger' {ledgerName} -> ledgerName) (\s@ListJournalKinesisStreamsForLedger' {} a -> s {ledgerName = a} :: ListJournalKinesisStreamsForLedger)

instance
  Core.AWSRequest
    ListJournalKinesisStreamsForLedger
  where
  type
    AWSResponse ListJournalKinesisStreamsForLedger =
      ListJournalKinesisStreamsForLedgerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJournalKinesisStreamsForLedgerResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Streams" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListJournalKinesisStreamsForLedger
  where
  hashWithSalt
    _salt
    ListJournalKinesisStreamsForLedger' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` ledgerName

instance
  Prelude.NFData
    ListJournalKinesisStreamsForLedger
  where
  rnf ListJournalKinesisStreamsForLedger' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ledgerName

instance
  Data.ToHeaders
    ListJournalKinesisStreamsForLedger
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    ListJournalKinesisStreamsForLedger
  where
  toPath ListJournalKinesisStreamsForLedger' {..} =
    Prelude.mconcat
      [ "/ledgers/",
        Data.toBS ledgerName,
        "/journal-kinesis-streams"
      ]

instance
  Data.ToQuery
    ListJournalKinesisStreamsForLedger
  where
  toQuery ListJournalKinesisStreamsForLedger' {..} =
    Prelude.mconcat
      [ "max_results" Data.=: maxResults,
        "next_token" Data.=: nextToken
      ]

-- | /See:/ 'newListJournalKinesisStreamsForLedgerResponse' smart constructor.
data ListJournalKinesisStreamsForLedgerResponse = ListJournalKinesisStreamsForLedgerResponse'
  { -- | -   If @NextToken@ is empty, the last page of results has been processed
    --     and there are no more results to be retrieved.
    --
    -- -   If @NextToken@ is /not/ empty, more results are available. To
    --     retrieve the next page of results, use the value of @NextToken@ in a
    --     subsequent @ListJournalKinesisStreamsForLedger@ call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The array of QLDB journal stream descriptors that are associated with
    -- the given ledger.
    streams :: Prelude.Maybe [JournalKinesisStreamDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJournalKinesisStreamsForLedgerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJournalKinesisStreamsForLedgerResponse_nextToken' - -   If @NextToken@ is empty, the last page of results has been processed
--     and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, more results are available. To
--     retrieve the next page of results, use the value of @NextToken@ in a
--     subsequent @ListJournalKinesisStreamsForLedger@ call.
--
-- 'streams', 'listJournalKinesisStreamsForLedgerResponse_streams' - The array of QLDB journal stream descriptors that are associated with
-- the given ledger.
--
-- 'httpStatus', 'listJournalKinesisStreamsForLedgerResponse_httpStatus' - The response's http status code.
newListJournalKinesisStreamsForLedgerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJournalKinesisStreamsForLedgerResponse
newListJournalKinesisStreamsForLedgerResponse
  pHttpStatus_ =
    ListJournalKinesisStreamsForLedgerResponse'
      { nextToken =
          Prelude.Nothing,
        streams = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | -   If @NextToken@ is empty, the last page of results has been processed
--     and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, more results are available. To
--     retrieve the next page of results, use the value of @NextToken@ in a
--     subsequent @ListJournalKinesisStreamsForLedger@ call.
listJournalKinesisStreamsForLedgerResponse_nextToken :: Lens.Lens' ListJournalKinesisStreamsForLedgerResponse (Prelude.Maybe Prelude.Text)
listJournalKinesisStreamsForLedgerResponse_nextToken = Lens.lens (\ListJournalKinesisStreamsForLedgerResponse' {nextToken} -> nextToken) (\s@ListJournalKinesisStreamsForLedgerResponse' {} a -> s {nextToken = a} :: ListJournalKinesisStreamsForLedgerResponse)

-- | The array of QLDB journal stream descriptors that are associated with
-- the given ledger.
listJournalKinesisStreamsForLedgerResponse_streams :: Lens.Lens' ListJournalKinesisStreamsForLedgerResponse (Prelude.Maybe [JournalKinesisStreamDescription])
listJournalKinesisStreamsForLedgerResponse_streams = Lens.lens (\ListJournalKinesisStreamsForLedgerResponse' {streams} -> streams) (\s@ListJournalKinesisStreamsForLedgerResponse' {} a -> s {streams = a} :: ListJournalKinesisStreamsForLedgerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listJournalKinesisStreamsForLedgerResponse_httpStatus :: Lens.Lens' ListJournalKinesisStreamsForLedgerResponse Prelude.Int
listJournalKinesisStreamsForLedgerResponse_httpStatus = Lens.lens (\ListJournalKinesisStreamsForLedgerResponse' {httpStatus} -> httpStatus) (\s@ListJournalKinesisStreamsForLedgerResponse' {} a -> s {httpStatus = a} :: ListJournalKinesisStreamsForLedgerResponse)

instance
  Prelude.NFData
    ListJournalKinesisStreamsForLedgerResponse
  where
  rnf ListJournalKinesisStreamsForLedgerResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streams
      `Prelude.seq` Prelude.rnf httpStatus
