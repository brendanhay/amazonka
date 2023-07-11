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
-- Module      : Amazonka.TimeStreamQuery.ListScheduledQueries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of all scheduled queries in the caller\'s Amazon account and
-- Region. @ListScheduledQueries@ is eventually consistent.
--
-- This operation returns paginated results.
module Amazonka.TimeStreamQuery.ListScheduledQueries
  ( -- * Creating a Request
    ListScheduledQueries (..),
    newListScheduledQueries,

    -- * Request Lenses
    listScheduledQueries_maxResults,
    listScheduledQueries_nextToken,

    -- * Destructuring the Response
    ListScheduledQueriesResponse (..),
    newListScheduledQueriesResponse,

    -- * Response Lenses
    listScheduledQueriesResponse_nextToken,
    listScheduledQueriesResponse_httpStatus,
    listScheduledQueriesResponse_scheduledQueries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newListScheduledQueries' smart constructor.
data ListScheduledQueries = ListScheduledQueries'
  { -- | The maximum number of items to return in the output. If the total number
    -- of items available is more than the value specified, a @NextToken@ is
    -- provided in the output. To resume pagination, provide the @NextToken@
    -- value as the argument to the subsequent call to
    -- @ListScheduledQueriesRequest@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token to resume pagination.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduledQueries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listScheduledQueries_maxResults' - The maximum number of items to return in the output. If the total number
-- of items available is more than the value specified, a @NextToken@ is
-- provided in the output. To resume pagination, provide the @NextToken@
-- value as the argument to the subsequent call to
-- @ListScheduledQueriesRequest@.
--
-- 'nextToken', 'listScheduledQueries_nextToken' - A pagination token to resume pagination.
newListScheduledQueries ::
  ListScheduledQueries
newListScheduledQueries =
  ListScheduledQueries'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of items to return in the output. If the total number
-- of items available is more than the value specified, a @NextToken@ is
-- provided in the output. To resume pagination, provide the @NextToken@
-- value as the argument to the subsequent call to
-- @ListScheduledQueriesRequest@.
listScheduledQueries_maxResults :: Lens.Lens' ListScheduledQueries (Prelude.Maybe Prelude.Natural)
listScheduledQueries_maxResults = Lens.lens (\ListScheduledQueries' {maxResults} -> maxResults) (\s@ListScheduledQueries' {} a -> s {maxResults = a} :: ListScheduledQueries)

-- | A pagination token to resume pagination.
listScheduledQueries_nextToken :: Lens.Lens' ListScheduledQueries (Prelude.Maybe Prelude.Text)
listScheduledQueries_nextToken = Lens.lens (\ListScheduledQueries' {nextToken} -> nextToken) (\s@ListScheduledQueries' {} a -> s {nextToken = a} :: ListScheduledQueries)

instance Core.AWSPager ListScheduledQueries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScheduledQueriesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listScheduledQueriesResponse_scheduledQueries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listScheduledQueries_nextToken
          Lens..~ rs
          Lens.^? listScheduledQueriesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListScheduledQueries where
  type
    AWSResponse ListScheduledQueries =
      ListScheduledQueriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScheduledQueriesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ScheduledQueries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListScheduledQueries where
  hashWithSalt _salt ListScheduledQueries' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListScheduledQueries where
  rnf ListScheduledQueries' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListScheduledQueries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.ListScheduledQueries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListScheduledQueries where
  toJSON ListScheduledQueries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListScheduledQueries where
  toPath = Prelude.const "/"

instance Data.ToQuery ListScheduledQueries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListScheduledQueriesResponse' smart constructor.
data ListScheduledQueriesResponse = ListScheduledQueriesResponse'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of scheduled queries.
    scheduledQueries :: [ScheduledQuery]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScheduledQueriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScheduledQueriesResponse_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'httpStatus', 'listScheduledQueriesResponse_httpStatus' - The response's http status code.
--
-- 'scheduledQueries', 'listScheduledQueriesResponse_scheduledQueries' - A list of scheduled queries.
newListScheduledQueriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListScheduledQueriesResponse
newListScheduledQueriesResponse pHttpStatus_ =
  ListScheduledQueriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      scheduledQueries = Prelude.mempty
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listScheduledQueriesResponse_nextToken :: Lens.Lens' ListScheduledQueriesResponse (Prelude.Maybe Prelude.Text)
listScheduledQueriesResponse_nextToken = Lens.lens (\ListScheduledQueriesResponse' {nextToken} -> nextToken) (\s@ListScheduledQueriesResponse' {} a -> s {nextToken = a} :: ListScheduledQueriesResponse)

-- | The response's http status code.
listScheduledQueriesResponse_httpStatus :: Lens.Lens' ListScheduledQueriesResponse Prelude.Int
listScheduledQueriesResponse_httpStatus = Lens.lens (\ListScheduledQueriesResponse' {httpStatus} -> httpStatus) (\s@ListScheduledQueriesResponse' {} a -> s {httpStatus = a} :: ListScheduledQueriesResponse)

-- | A list of scheduled queries.
listScheduledQueriesResponse_scheduledQueries :: Lens.Lens' ListScheduledQueriesResponse [ScheduledQuery]
listScheduledQueriesResponse_scheduledQueries = Lens.lens (\ListScheduledQueriesResponse' {scheduledQueries} -> scheduledQueries) (\s@ListScheduledQueriesResponse' {} a -> s {scheduledQueries = a} :: ListScheduledQueriesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListScheduledQueriesResponse where
  rnf ListScheduledQueriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf scheduledQueries
