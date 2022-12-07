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
-- Module      : Amazonka.Budgets.DescribeBudgetActionsForBudget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for a budget.
--
-- This operation returns paginated results.
module Amazonka.Budgets.DescribeBudgetActionsForBudget
  ( -- * Creating a Request
    DescribeBudgetActionsForBudget (..),
    newDescribeBudgetActionsForBudget,

    -- * Request Lenses
    describeBudgetActionsForBudget_nextToken,
    describeBudgetActionsForBudget_maxResults,
    describeBudgetActionsForBudget_accountId,
    describeBudgetActionsForBudget_budgetName,

    -- * Destructuring the Response
    DescribeBudgetActionsForBudgetResponse (..),
    newDescribeBudgetActionsForBudgetResponse,

    -- * Response Lenses
    describeBudgetActionsForBudgetResponse_nextToken,
    describeBudgetActionsForBudgetResponse_httpStatus,
    describeBudgetActionsForBudgetResponse_actions,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBudgetActionsForBudget' smart constructor.
data DescribeBudgetActionsForBudget = DescribeBudgetActionsForBudget'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetActionsForBudget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgetActionsForBudget_nextToken' - Undocumented member.
--
-- 'maxResults', 'describeBudgetActionsForBudget_maxResults' - Undocumented member.
--
-- 'accountId', 'describeBudgetActionsForBudget_accountId' - Undocumented member.
--
-- 'budgetName', 'describeBudgetActionsForBudget_budgetName' - Undocumented member.
newDescribeBudgetActionsForBudget ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  DescribeBudgetActionsForBudget
newDescribeBudgetActionsForBudget
  pAccountId_
  pBudgetName_ =
    DescribeBudgetActionsForBudget'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_
      }

-- | Undocumented member.
describeBudgetActionsForBudget_nextToken :: Lens.Lens' DescribeBudgetActionsForBudget (Prelude.Maybe Prelude.Text)
describeBudgetActionsForBudget_nextToken = Lens.lens (\DescribeBudgetActionsForBudget' {nextToken} -> nextToken) (\s@DescribeBudgetActionsForBudget' {} a -> s {nextToken = a} :: DescribeBudgetActionsForBudget)

-- | Undocumented member.
describeBudgetActionsForBudget_maxResults :: Lens.Lens' DescribeBudgetActionsForBudget (Prelude.Maybe Prelude.Natural)
describeBudgetActionsForBudget_maxResults = Lens.lens (\DescribeBudgetActionsForBudget' {maxResults} -> maxResults) (\s@DescribeBudgetActionsForBudget' {} a -> s {maxResults = a} :: DescribeBudgetActionsForBudget)

-- | Undocumented member.
describeBudgetActionsForBudget_accountId :: Lens.Lens' DescribeBudgetActionsForBudget Prelude.Text
describeBudgetActionsForBudget_accountId = Lens.lens (\DescribeBudgetActionsForBudget' {accountId} -> accountId) (\s@DescribeBudgetActionsForBudget' {} a -> s {accountId = a} :: DescribeBudgetActionsForBudget)

-- | Undocumented member.
describeBudgetActionsForBudget_budgetName :: Lens.Lens' DescribeBudgetActionsForBudget Prelude.Text
describeBudgetActionsForBudget_budgetName = Lens.lens (\DescribeBudgetActionsForBudget' {budgetName} -> budgetName) (\s@DescribeBudgetActionsForBudget' {} a -> s {budgetName = a} :: DescribeBudgetActionsForBudget)

instance Core.AWSPager DescribeBudgetActionsForBudget where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetActionsForBudgetResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeBudgetActionsForBudgetResponse_actions
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBudgetActionsForBudget_nextToken
          Lens..~ rs
          Lens.^? describeBudgetActionsForBudgetResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeBudgetActionsForBudget
  where
  type
    AWSResponse DescribeBudgetActionsForBudget =
      DescribeBudgetActionsForBudgetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForBudgetResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Actions" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeBudgetActionsForBudget
  where
  hashWithSalt
    _salt
    DescribeBudgetActionsForBudget' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` budgetName

instance
  Prelude.NFData
    DescribeBudgetActionsForBudget
  where
  rnf DescribeBudgetActionsForBudget' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName

instance
  Data.ToHeaders
    DescribeBudgetActionsForBudget
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForBudget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBudgetActionsForBudget where
  toJSON DescribeBudgetActionsForBudget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName)
          ]
      )

instance Data.ToPath DescribeBudgetActionsForBudget where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBudgetActionsForBudget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBudgetActionsForBudgetResponse' smart constructor.
data DescribeBudgetActionsForBudgetResponse = DescribeBudgetActionsForBudgetResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of the budget action resources information.
    actions :: [Action]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetActionsForBudgetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgetActionsForBudgetResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeBudgetActionsForBudgetResponse_httpStatus' - The response's http status code.
--
-- 'actions', 'describeBudgetActionsForBudgetResponse_actions' - A list of the budget action resources information.
newDescribeBudgetActionsForBudgetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetActionsForBudgetResponse
newDescribeBudgetActionsForBudgetResponse
  pHttpStatus_ =
    DescribeBudgetActionsForBudgetResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        actions = Prelude.mempty
      }

-- | Undocumented member.
describeBudgetActionsForBudgetResponse_nextToken :: Lens.Lens' DescribeBudgetActionsForBudgetResponse (Prelude.Maybe Prelude.Text)
describeBudgetActionsForBudgetResponse_nextToken = Lens.lens (\DescribeBudgetActionsForBudgetResponse' {nextToken} -> nextToken) (\s@DescribeBudgetActionsForBudgetResponse' {} a -> s {nextToken = a} :: DescribeBudgetActionsForBudgetResponse)

-- | The response's http status code.
describeBudgetActionsForBudgetResponse_httpStatus :: Lens.Lens' DescribeBudgetActionsForBudgetResponse Prelude.Int
describeBudgetActionsForBudgetResponse_httpStatus = Lens.lens (\DescribeBudgetActionsForBudgetResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetActionsForBudgetResponse' {} a -> s {httpStatus = a} :: DescribeBudgetActionsForBudgetResponse)

-- | A list of the budget action resources information.
describeBudgetActionsForBudgetResponse_actions :: Lens.Lens' DescribeBudgetActionsForBudgetResponse [Action]
describeBudgetActionsForBudgetResponse_actions = Lens.lens (\DescribeBudgetActionsForBudgetResponse' {actions} -> actions) (\s@DescribeBudgetActionsForBudgetResponse' {} a -> s {actions = a} :: DescribeBudgetActionsForBudgetResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeBudgetActionsForBudgetResponse
  where
  rnf DescribeBudgetActionsForBudgetResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf actions
