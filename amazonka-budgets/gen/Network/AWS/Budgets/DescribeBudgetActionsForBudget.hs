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
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForBudget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for a budget.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForBudget
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

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForBudgetResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Actions" Core..!@ Prelude.mempty)
      )

instance
  Prelude.Hashable
    DescribeBudgetActionsForBudget

instance
  Prelude.NFData
    DescribeBudgetActionsForBudget

instance
  Core.ToHeaders
    DescribeBudgetActionsForBudget
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionsForBudget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeBudgetActionsForBudget where
  toJSON DescribeBudgetActionsForBudget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("AccountId" Core..= accountId),
            Prelude.Just ("BudgetName" Core..= budgetName)
          ]
      )

instance Core.ToPath DescribeBudgetActionsForBudget where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeBudgetActionsForBudget where
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
describeBudgetActionsForBudgetResponse_actions = Lens.lens (\DescribeBudgetActionsForBudgetResponse' {actions} -> actions) (\s@DescribeBudgetActionsForBudgetResponse' {} a -> s {actions = a} :: DescribeBudgetActionsForBudgetResponse) Prelude.. Lens._Coerce

instance
  Prelude.NFData
    DescribeBudgetActionsForBudgetResponse
