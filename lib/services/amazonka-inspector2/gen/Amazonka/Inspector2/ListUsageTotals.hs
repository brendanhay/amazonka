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
-- Module      : Amazonka.Inspector2.ListUsageTotals
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon Inspector usage totals over the last 30 days.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.ListUsageTotals
  ( -- * Creating a Request
    ListUsageTotals (..),
    newListUsageTotals,

    -- * Request Lenses
    listUsageTotals_accountIds,
    listUsageTotals_nextToken,
    listUsageTotals_maxResults,

    -- * Destructuring the Response
    ListUsageTotalsResponse (..),
    newListUsageTotalsResponse,

    -- * Response Lenses
    listUsageTotalsResponse_nextToken,
    listUsageTotalsResponse_totals,
    listUsageTotalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListUsageTotals' smart constructor.
data ListUsageTotals = ListUsageTotals'
  { -- | The Amazon Web Services account IDs to retrieve usage totals for.
    accountIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsageTotals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountIds', 'listUsageTotals_accountIds' - The Amazon Web Services account IDs to retrieve usage totals for.
--
-- 'nextToken', 'listUsageTotals_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'maxResults', 'listUsageTotals_maxResults' - The maximum number of results to return in the response.
newListUsageTotals ::
  ListUsageTotals
newListUsageTotals =
  ListUsageTotals'
    { accountIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The Amazon Web Services account IDs to retrieve usage totals for.
listUsageTotals_accountIds :: Lens.Lens' ListUsageTotals (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listUsageTotals_accountIds = Lens.lens (\ListUsageTotals' {accountIds} -> accountIds) (\s@ListUsageTotals' {} a -> s {accountIds = a} :: ListUsageTotals) Prelude.. Lens.mapping Lens.coerced

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listUsageTotals_nextToken :: Lens.Lens' ListUsageTotals (Prelude.Maybe Prelude.Text)
listUsageTotals_nextToken = Lens.lens (\ListUsageTotals' {nextToken} -> nextToken) (\s@ListUsageTotals' {} a -> s {nextToken = a} :: ListUsageTotals)

-- | The maximum number of results to return in the response.
listUsageTotals_maxResults :: Lens.Lens' ListUsageTotals (Prelude.Maybe Prelude.Natural)
listUsageTotals_maxResults = Lens.lens (\ListUsageTotals' {maxResults} -> maxResults) (\s@ListUsageTotals' {} a -> s {maxResults = a} :: ListUsageTotals)

instance Core.AWSPager ListUsageTotals where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listUsageTotalsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listUsageTotalsResponse_totals Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listUsageTotals_nextToken
          Lens..~ rs
          Lens.^? listUsageTotalsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListUsageTotals where
  type
    AWSResponse ListUsageTotals =
      ListUsageTotalsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListUsageTotalsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "totals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListUsageTotals where
  hashWithSalt _salt ListUsageTotals' {..} =
    _salt `Prelude.hashWithSalt` accountIds
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListUsageTotals where
  rnf ListUsageTotals' {..} =
    Prelude.rnf accountIds
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListUsageTotals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListUsageTotals where
  toJSON ListUsageTotals' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("accountIds" Core..=) Prelude.<$> accountIds,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListUsageTotals where
  toPath = Prelude.const "/usage/list"

instance Core.ToQuery ListUsageTotals where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListUsageTotalsResponse' smart constructor.
data ListUsageTotalsResponse = ListUsageTotalsResponse'
  { -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An object with details on the total usage for the requested account.
    totals :: Prelude.Maybe [UsageTotal],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListUsageTotalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listUsageTotalsResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'totals', 'listUsageTotalsResponse_totals' - An object with details on the total usage for the requested account.
--
-- 'httpStatus', 'listUsageTotalsResponse_httpStatus' - The response's http status code.
newListUsageTotalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListUsageTotalsResponse
newListUsageTotalsResponse pHttpStatus_ =
  ListUsageTotalsResponse'
    { nextToken =
        Prelude.Nothing,
      totals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
listUsageTotalsResponse_nextToken :: Lens.Lens' ListUsageTotalsResponse (Prelude.Maybe Prelude.Text)
listUsageTotalsResponse_nextToken = Lens.lens (\ListUsageTotalsResponse' {nextToken} -> nextToken) (\s@ListUsageTotalsResponse' {} a -> s {nextToken = a} :: ListUsageTotalsResponse)

-- | An object with details on the total usage for the requested account.
listUsageTotalsResponse_totals :: Lens.Lens' ListUsageTotalsResponse (Prelude.Maybe [UsageTotal])
listUsageTotalsResponse_totals = Lens.lens (\ListUsageTotalsResponse' {totals} -> totals) (\s@ListUsageTotalsResponse' {} a -> s {totals = a} :: ListUsageTotalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listUsageTotalsResponse_httpStatus :: Lens.Lens' ListUsageTotalsResponse Prelude.Int
listUsageTotalsResponse_httpStatus = Lens.lens (\ListUsageTotalsResponse' {httpStatus} -> httpStatus) (\s@ListUsageTotalsResponse' {} a -> s {httpStatus = a} :: ListUsageTotalsResponse)

instance Prelude.NFData ListUsageTotalsResponse where
  rnf ListUsageTotalsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf totals
      `Prelude.seq` Prelude.rnf httpStatus
