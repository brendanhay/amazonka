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
-- Module      : Amazonka.Budgets.DescribeBudgetActionHistories
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action history detail.
--
-- This operation returns paginated results.
module Amazonka.Budgets.DescribeBudgetActionHistories
  ( -- * Creating a Request
    DescribeBudgetActionHistories (..),
    newDescribeBudgetActionHistories,

    -- * Request Lenses
    describeBudgetActionHistories_nextToken,
    describeBudgetActionHistories_maxResults,
    describeBudgetActionHistories_timePeriod,
    describeBudgetActionHistories_accountId,
    describeBudgetActionHistories_budgetName,
    describeBudgetActionHistories_actionId,

    -- * Destructuring the Response
    DescribeBudgetActionHistoriesResponse (..),
    newDescribeBudgetActionHistoriesResponse,

    -- * Response Lenses
    describeBudgetActionHistoriesResponse_nextToken,
    describeBudgetActionHistoriesResponse_httpStatus,
    describeBudgetActionHistoriesResponse_actionHistories,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBudgetActionHistories' smart constructor.
data DescribeBudgetActionHistories = DescribeBudgetActionHistories'
  { nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural,
    timePeriod :: Prelude.Maybe TimePeriod,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetActionHistories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgetActionHistories_nextToken' - Undocumented member.
--
-- 'maxResults', 'describeBudgetActionHistories_maxResults' - Undocumented member.
--
-- 'timePeriod', 'describeBudgetActionHistories_timePeriod' - Undocumented member.
--
-- 'accountId', 'describeBudgetActionHistories_accountId' - Undocumented member.
--
-- 'budgetName', 'describeBudgetActionHistories_budgetName' - Undocumented member.
--
-- 'actionId', 'describeBudgetActionHistories_actionId' - A system-generated universally unique identifier (UUID) for the action.
newDescribeBudgetActionHistories ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
  DescribeBudgetActionHistories
newDescribeBudgetActionHistories
  pAccountId_
  pBudgetName_
  pActionId_ =
    DescribeBudgetActionHistories'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        timePeriod = Prelude.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_
      }

-- | Undocumented member.
describeBudgetActionHistories_nextToken :: Lens.Lens' DescribeBudgetActionHistories (Prelude.Maybe Prelude.Text)
describeBudgetActionHistories_nextToken = Lens.lens (\DescribeBudgetActionHistories' {nextToken} -> nextToken) (\s@DescribeBudgetActionHistories' {} a -> s {nextToken = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_maxResults :: Lens.Lens' DescribeBudgetActionHistories (Prelude.Maybe Prelude.Natural)
describeBudgetActionHistories_maxResults = Lens.lens (\DescribeBudgetActionHistories' {maxResults} -> maxResults) (\s@DescribeBudgetActionHistories' {} a -> s {maxResults = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_timePeriod :: Lens.Lens' DescribeBudgetActionHistories (Prelude.Maybe TimePeriod)
describeBudgetActionHistories_timePeriod = Lens.lens (\DescribeBudgetActionHistories' {timePeriod} -> timePeriod) (\s@DescribeBudgetActionHistories' {} a -> s {timePeriod = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_accountId :: Lens.Lens' DescribeBudgetActionHistories Prelude.Text
describeBudgetActionHistories_accountId = Lens.lens (\DescribeBudgetActionHistories' {accountId} -> accountId) (\s@DescribeBudgetActionHistories' {} a -> s {accountId = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_budgetName :: Lens.Lens' DescribeBudgetActionHistories Prelude.Text
describeBudgetActionHistories_budgetName = Lens.lens (\DescribeBudgetActionHistories' {budgetName} -> budgetName) (\s@DescribeBudgetActionHistories' {} a -> s {budgetName = a} :: DescribeBudgetActionHistories)

-- | A system-generated universally unique identifier (UUID) for the action.
describeBudgetActionHistories_actionId :: Lens.Lens' DescribeBudgetActionHistories Prelude.Text
describeBudgetActionHistories_actionId = Lens.lens (\DescribeBudgetActionHistories' {actionId} -> actionId) (\s@DescribeBudgetActionHistories' {} a -> s {actionId = a} :: DescribeBudgetActionHistories)

instance Core.AWSPager DescribeBudgetActionHistories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetActionHistoriesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. describeBudgetActionHistoriesResponse_actionHistories
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeBudgetActionHistories_nextToken
          Lens..~ rs
          Lens.^? describeBudgetActionHistoriesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeBudgetActionHistories
  where
  type
    AWSResponse DescribeBudgetActionHistories =
      DescribeBudgetActionHistoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionHistoriesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "ActionHistories"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    DescribeBudgetActionHistories
  where
  hashWithSalt _salt DescribeBudgetActionHistories' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` timePeriod
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` actionId

instance Prelude.NFData DescribeBudgetActionHistories where
  rnf DescribeBudgetActionHistories' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf timePeriod
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf actionId

instance Data.ToHeaders DescribeBudgetActionHistories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionHistories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBudgetActionHistories where
  toJSON DescribeBudgetActionHistories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("TimePeriod" Data..=) Prelude.<$> timePeriod,
            Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("ActionId" Data..= actionId)
          ]
      )

instance Data.ToPath DescribeBudgetActionHistories where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBudgetActionHistories where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBudgetActionHistoriesResponse' smart constructor.
data DescribeBudgetActionHistoriesResponse = DescribeBudgetActionHistoriesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The historical record of the budget action resource.
    actionHistories :: [ActionHistory]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetActionHistoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeBudgetActionHistoriesResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeBudgetActionHistoriesResponse_httpStatus' - The response's http status code.
--
-- 'actionHistories', 'describeBudgetActionHistoriesResponse_actionHistories' - The historical record of the budget action resource.
newDescribeBudgetActionHistoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetActionHistoriesResponse
newDescribeBudgetActionHistoriesResponse pHttpStatus_ =
  DescribeBudgetActionHistoriesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      actionHistories = Prelude.mempty
    }

-- | Undocumented member.
describeBudgetActionHistoriesResponse_nextToken :: Lens.Lens' DescribeBudgetActionHistoriesResponse (Prelude.Maybe Prelude.Text)
describeBudgetActionHistoriesResponse_nextToken = Lens.lens (\DescribeBudgetActionHistoriesResponse' {nextToken} -> nextToken) (\s@DescribeBudgetActionHistoriesResponse' {} a -> s {nextToken = a} :: DescribeBudgetActionHistoriesResponse)

-- | The response's http status code.
describeBudgetActionHistoriesResponse_httpStatus :: Lens.Lens' DescribeBudgetActionHistoriesResponse Prelude.Int
describeBudgetActionHistoriesResponse_httpStatus = Lens.lens (\DescribeBudgetActionHistoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetActionHistoriesResponse' {} a -> s {httpStatus = a} :: DescribeBudgetActionHistoriesResponse)

-- | The historical record of the budget action resource.
describeBudgetActionHistoriesResponse_actionHistories :: Lens.Lens' DescribeBudgetActionHistoriesResponse [ActionHistory]
describeBudgetActionHistoriesResponse_actionHistories = Lens.lens (\DescribeBudgetActionHistoriesResponse' {actionHistories} -> actionHistories) (\s@DescribeBudgetActionHistoriesResponse' {} a -> s {actionHistories = a} :: DescribeBudgetActionHistoriesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeBudgetActionHistoriesResponse
  where
  rnf DescribeBudgetActionHistoriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf actionHistories
