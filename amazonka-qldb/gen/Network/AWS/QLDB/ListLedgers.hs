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
-- Module      : Network.AWS.QLDB.ListLedgers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of ledger summaries that are associated with the
-- current AWS account and Region.
--
-- This action returns a maximum of 100 items and is paginated so that you
-- can retrieve all the items by calling @ListLedgers@ multiple times.
module Network.AWS.QLDB.ListLedgers
  ( -- * Creating a Request
    ListLedgers (..),
    newListLedgers,

    -- * Request Lenses
    listLedgers_nextToken,
    listLedgers_maxResults,

    -- * Destructuring the Response
    ListLedgersResponse (..),
    newListLedgersResponse,

    -- * Response Lenses
    listLedgersResponse_nextToken,
    listLedgersResponse_ledgers,
    listLedgersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDB.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListLedgers' smart constructor.
data ListLedgers = ListLedgers'
  { -- | A pagination token, indicating that you want to retrieve the next page
    -- of results. If you received a value for @NextToken@ in the response from
    -- a previous @ListLedgers@ call, then you should use that value as input
    -- here.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a single @ListLedgers@
    -- request. (The actual number of results returned might be fewer.)
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLedgers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLedgers_nextToken' - A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListLedgers@ call, then you should use that value as input
-- here.
--
-- 'maxResults', 'listLedgers_maxResults' - The maximum number of results to return in a single @ListLedgers@
-- request. (The actual number of results returned might be fewer.)
newListLedgers ::
  ListLedgers
newListLedgers =
  ListLedgers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A pagination token, indicating that you want to retrieve the next page
-- of results. If you received a value for @NextToken@ in the response from
-- a previous @ListLedgers@ call, then you should use that value as input
-- here.
listLedgers_nextToken :: Lens.Lens' ListLedgers (Prelude.Maybe Prelude.Text)
listLedgers_nextToken = Lens.lens (\ListLedgers' {nextToken} -> nextToken) (\s@ListLedgers' {} a -> s {nextToken = a} :: ListLedgers)

-- | The maximum number of results to return in a single @ListLedgers@
-- request. (The actual number of results returned might be fewer.)
listLedgers_maxResults :: Lens.Lens' ListLedgers (Prelude.Maybe Prelude.Natural)
listLedgers_maxResults = Lens.lens (\ListLedgers' {maxResults} -> maxResults) (\s@ListLedgers' {} a -> s {maxResults = a} :: ListLedgers)

instance Core.AWSRequest ListLedgers where
  type AWSResponse ListLedgers = ListLedgersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLedgersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Ledgers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLedgers

instance Prelude.NFData ListLedgers

instance Core.ToHeaders ListLedgers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListLedgers where
  toPath = Prelude.const "/ledgers"

instance Core.ToQuery ListLedgers where
  toQuery ListLedgers' {..} =
    Prelude.mconcat
      [ "next_token" Core.=: nextToken,
        "max_results" Core.=: maxResults
      ]

-- | /See:/ 'newListLedgersResponse' smart constructor.
data ListLedgersResponse = ListLedgersResponse'
  { -- | A pagination token, indicating whether there are more results available:
    --
    -- -   If @NextToken@ is empty, then the last page of results has been
    --     processed and there are no more results to be retrieved.
    --
    -- -   If @NextToken@ is /not/ empty, then there are more results
    --     available. To retrieve the next page of results, use the value of
    --     @NextToken@ in a subsequent @ListLedgers@ call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The array of ledger summaries that are associated with the current AWS
    -- account and Region.
    ledgers :: Prelude.Maybe [LedgerSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLedgersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLedgersResponse_nextToken' - A pagination token, indicating whether there are more results available:
--
-- -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListLedgers@ call.
--
-- 'ledgers', 'listLedgersResponse_ledgers' - The array of ledger summaries that are associated with the current AWS
-- account and Region.
--
-- 'httpStatus', 'listLedgersResponse_httpStatus' - The response's http status code.
newListLedgersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLedgersResponse
newListLedgersResponse pHttpStatus_ =
  ListLedgersResponse'
    { nextToken = Prelude.Nothing,
      ledgers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token, indicating whether there are more results available:
--
-- -   If @NextToken@ is empty, then the last page of results has been
--     processed and there are no more results to be retrieved.
--
-- -   If @NextToken@ is /not/ empty, then there are more results
--     available. To retrieve the next page of results, use the value of
--     @NextToken@ in a subsequent @ListLedgers@ call.
listLedgersResponse_nextToken :: Lens.Lens' ListLedgersResponse (Prelude.Maybe Prelude.Text)
listLedgersResponse_nextToken = Lens.lens (\ListLedgersResponse' {nextToken} -> nextToken) (\s@ListLedgersResponse' {} a -> s {nextToken = a} :: ListLedgersResponse)

-- | The array of ledger summaries that are associated with the current AWS
-- account and Region.
listLedgersResponse_ledgers :: Lens.Lens' ListLedgersResponse (Prelude.Maybe [LedgerSummary])
listLedgersResponse_ledgers = Lens.lens (\ListLedgersResponse' {ledgers} -> ledgers) (\s@ListLedgersResponse' {} a -> s {ledgers = a} :: ListLedgersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listLedgersResponse_httpStatus :: Lens.Lens' ListLedgersResponse Prelude.Int
listLedgersResponse_httpStatus = Lens.lens (\ListLedgersResponse' {httpStatus} -> httpStatus) (\s@ListLedgersResponse' {} a -> s {httpStatus = a} :: ListLedgersResponse)

instance Prelude.NFData ListLedgersResponse
