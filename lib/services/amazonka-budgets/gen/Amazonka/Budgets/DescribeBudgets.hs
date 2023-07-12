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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    describeBudgets_maxResults,
    describeBudgets_nextToken,
    describeBudgets_accountId,

    -- * Destructuring the Response
    DescribeBudgetsResponse (..),
    newDescribeBudgetsResponse,

    -- * Response Lenses
    describeBudgetsResponse_budgets,
    describeBudgetsResponse_nextToken,
    describeBudgetsResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of DescribeBudgets
--
-- /See:/ 'newDescribeBudgets' smart constructor.
data DescribeBudgets = DescribeBudgets'
  { -- | An optional integer that represents how many entries a paginated
    -- response contains. The maximum is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that you include in your request to indicate the
    -- next set of results that you want to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'describeBudgets_maxResults' - An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
--
-- 'nextToken', 'describeBudgets_nextToken' - The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
--
-- 'accountId', 'describeBudgets_accountId' - The @accountId@ that is associated with the budgets that you want
-- descriptions of.
newDescribeBudgets ::
  -- | 'accountId'
  Prelude.Text ->
  DescribeBudgets
newDescribeBudgets pAccountId_ =
  DescribeBudgets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      accountId = pAccountId_
    }

-- | An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
describeBudgets_maxResults :: Lens.Lens' DescribeBudgets (Prelude.Maybe Prelude.Natural)
describeBudgets_maxResults = Lens.lens (\DescribeBudgets' {maxResults} -> maxResults) (\s@DescribeBudgets' {} a -> s {maxResults = a} :: DescribeBudgets)

-- | The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
describeBudgets_nextToken :: Lens.Lens' DescribeBudgets (Prelude.Maybe Prelude.Text)
describeBudgets_nextToken = Lens.lens (\DescribeBudgets' {nextToken} -> nextToken) (\s@DescribeBudgets' {} a -> s {nextToken = a} :: DescribeBudgets)

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
            Lens.^? describeBudgetsResponse_budgets
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> (x Data..?> "Budgets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBudgets where
  hashWithSalt _salt DescribeBudgets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData DescribeBudgets where
  rnf DescribeBudgets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountId

instance Data.ToHeaders DescribeBudgets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DescribeBudgets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBudgets where
  toJSON DescribeBudgets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("AccountId" Data..= accountId)
          ]
      )

instance Data.ToPath DescribeBudgets where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBudgets where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DescribeBudgets
--
-- /See:/ 'newDescribeBudgetsResponse' smart constructor.
data DescribeBudgetsResponse = DescribeBudgetsResponse'
  { -- | A list of budgets.
    budgets :: Prelude.Maybe [Budget],
    -- | The pagination token in the service response that indicates the next set
    -- of results that you can retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'budgets', 'describeBudgetsResponse_budgets' - A list of budgets.
--
-- 'nextToken', 'describeBudgetsResponse_nextToken' - The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
--
-- 'httpStatus', 'describeBudgetsResponse_httpStatus' - The response's http status code.
newDescribeBudgetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetsResponse
newDescribeBudgetsResponse pHttpStatus_ =
  DescribeBudgetsResponse'
    { budgets = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of budgets.
describeBudgetsResponse_budgets :: Lens.Lens' DescribeBudgetsResponse (Prelude.Maybe [Budget])
describeBudgetsResponse_budgets = Lens.lens (\DescribeBudgetsResponse' {budgets} -> budgets) (\s@DescribeBudgetsResponse' {} a -> s {budgets = a} :: DescribeBudgetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
describeBudgetsResponse_nextToken :: Lens.Lens' DescribeBudgetsResponse (Prelude.Maybe Prelude.Text)
describeBudgetsResponse_nextToken = Lens.lens (\DescribeBudgetsResponse' {nextToken} -> nextToken) (\s@DescribeBudgetsResponse' {} a -> s {nextToken = a} :: DescribeBudgetsResponse)

-- | The response's http status code.
describeBudgetsResponse_httpStatus :: Lens.Lens' DescribeBudgetsResponse Prelude.Int
describeBudgetsResponse_httpStatus = Lens.lens (\DescribeBudgetsResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetsResponse' {} a -> s {httpStatus = a} :: DescribeBudgetsResponse)

instance Prelude.NFData DescribeBudgetsResponse where
  rnf DescribeBudgetsResponse' {..} =
    Prelude.rnf budgets
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
