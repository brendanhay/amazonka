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
-- Module      : Amazonka.Budgets.DescribeBudgets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the budgets that are associated with an account.
--
-- The Request Syntax section shows the @BudgetLimit@ syntax. For
-- @PlannedBudgetLimits@, see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_DescribeBudgets.html#API_DescribeBudgets_Examples Examples>
-- section.
--
-- This operation returns paginated results.
module Amazonka.Budgets.DescribeBudgets
  ( -- * Creating a Request
    DescribeBudgets (..),
    newDescribeBudgets,

    -- * Request Lenses
    describeBudgets_nextToken,
    describeBudgets_maxResults,
    describeBudgets_accountId,

    -- * Destructuring the Response
    DescribeBudgetsResponse (..),
    newDescribeBudgetsResponse,

    -- * Response Lenses
    describeBudgetsResponse_nextToken,
    describeBudgetsResponse_budgets,
    describeBudgetsResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of DescribeBudgets
--
-- /See:/ 'newDescribeBudgets' smart constructor.
data DescribeBudgets = DescribeBudgets'
  { -- | The pagination token that you include in your request to indicate the
    -- next set of results that you want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional integer that represents how many entries a paginated
    -- response contains. The maximum is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The @accountId@ that is associated with the budgets that you want
    -- descriptions of.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgets_nextToken' - The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
--
-- 'maxResults', 'describeBudgets_maxResults' - An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
--
-- 'accountId', 'describeBudgets_accountId' - The @accountId@ that is associated with the budgets that you want
-- descriptions of.
newDescribeBudgets ::
  -- | 'accountId'
  Prelude.Text ->
  DescribeBudgets
newDescribeBudgets pAccountId_ =
  DescribeBudgets'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
describeBudgets_nextToken :: Lens.Lens' DescribeBudgets (Prelude.Maybe Prelude.Text)
describeBudgets_nextToken = Lens.lens (\DescribeBudgets' {nextToken} -> nextToken) (\s@DescribeBudgets' {} a -> s {nextToken = a} :: DescribeBudgets)

-- | An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
describeBudgets_maxResults :: Lens.Lens' DescribeBudgets (Prelude.Maybe Prelude.Natural)
describeBudgets_maxResults = Lens.lens (\DescribeBudgets' {maxResults} -> maxResults) (\s@DescribeBudgets' {} a -> s {maxResults = a} :: DescribeBudgets)

-- | The @accountId@ that is associated with the budgets that you want
-- descriptions of.
describeBudgets_accountId :: Lens.Lens' DescribeBudgets Prelude.Text
describeBudgets_accountId = Lens.lens (\DescribeBudgets' {accountId} -> accountId) (\s@DescribeBudgets' {} a -> s {accountId = a} :: DescribeBudgets)

instance Core.AWSPager DescribeBudgets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeBudgetsResponse_budgets Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBudgets_nextToken
          Lens..~ rs
          Lens.^? describeBudgetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeBudgets where
  type
    AWSResponse DescribeBudgets =
      DescribeBudgetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Budgets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBudgets where
  hashWithSalt _salt DescribeBudgets' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData DescribeBudgets where
  rnf DescribeBudgets' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf accountId

instance Core.ToHeaders DescribeBudgets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeBudgets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeBudgets where
  toJSON DescribeBudgets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("AccountId" Core..= accountId)
          ]
      )

instance Core.ToPath DescribeBudgets where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeBudgets where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DescribeBudgets
--
-- /See:/ 'newDescribeBudgetsResponse' smart constructor.
data DescribeBudgetsResponse = DescribeBudgetsResponse'
  { -- | The pagination token in the service response that indicates the next set
    -- of results that you can retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of budgets.
    budgets :: Prelude.Maybe [Budget],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgetsResponse_nextToken' - The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
--
-- 'budgets', 'describeBudgetsResponse_budgets' - A list of budgets.
--
-- 'httpStatus', 'describeBudgetsResponse_httpStatus' - The response's http status code.
newDescribeBudgetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetsResponse
newDescribeBudgetsResponse pHttpStatus_ =
  DescribeBudgetsResponse'
    { nextToken =
        Prelude.Nothing,
      budgets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
describeBudgetsResponse_nextToken :: Lens.Lens' DescribeBudgetsResponse (Prelude.Maybe Prelude.Text)
describeBudgetsResponse_nextToken = Lens.lens (\DescribeBudgetsResponse' {nextToken} -> nextToken) (\s@DescribeBudgetsResponse' {} a -> s {nextToken = a} :: DescribeBudgetsResponse)

-- | A list of budgets.
describeBudgetsResponse_budgets :: Lens.Lens' DescribeBudgetsResponse (Prelude.Maybe [Budget])
describeBudgetsResponse_budgets = Lens.lens (\DescribeBudgetsResponse' {budgets} -> budgets) (\s@DescribeBudgetsResponse' {} a -> s {budgets = a} :: DescribeBudgetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeBudgetsResponse_httpStatus :: Lens.Lens' DescribeBudgetsResponse Prelude.Int
describeBudgetsResponse_httpStatus = Lens.lens (\DescribeBudgetsResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetsResponse' {} a -> s {httpStatus = a} :: DescribeBudgetsResponse)

instance Prelude.NFData DescribeBudgetsResponse where
  rnf DescribeBudgetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf budgets
      `Prelude.seq` Prelude.rnf httpStatus
