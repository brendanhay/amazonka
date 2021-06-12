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
-- Module      : Network.AWS.Budgets.DescribeBudgetActionHistories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action history detail.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionHistories
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

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBudgetActionHistories' smart constructor.
data DescribeBudgetActionHistories = DescribeBudgetActionHistories'
  { nextToken :: Core.Maybe Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    timePeriod :: Core.Maybe TimePeriod,
    accountId :: Core.Text,
    budgetName :: Core.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'actionId'
  Core.Text ->
  DescribeBudgetActionHistories
newDescribeBudgetActionHistories
  pAccountId_
  pBudgetName_
  pActionId_ =
    DescribeBudgetActionHistories'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        timePeriod = Core.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_
      }

-- | Undocumented member.
describeBudgetActionHistories_nextToken :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Core.Text)
describeBudgetActionHistories_nextToken = Lens.lens (\DescribeBudgetActionHistories' {nextToken} -> nextToken) (\s@DescribeBudgetActionHistories' {} a -> s {nextToken = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_maxResults :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Core.Natural)
describeBudgetActionHistories_maxResults = Lens.lens (\DescribeBudgetActionHistories' {maxResults} -> maxResults) (\s@DescribeBudgetActionHistories' {} a -> s {maxResults = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_timePeriod :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe TimePeriod)
describeBudgetActionHistories_timePeriod = Lens.lens (\DescribeBudgetActionHistories' {timePeriod} -> timePeriod) (\s@DescribeBudgetActionHistories' {} a -> s {timePeriod = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_accountId :: Lens.Lens' DescribeBudgetActionHistories Core.Text
describeBudgetActionHistories_accountId = Lens.lens (\DescribeBudgetActionHistories' {accountId} -> accountId) (\s@DescribeBudgetActionHistories' {} a -> s {accountId = a} :: DescribeBudgetActionHistories)

-- | Undocumented member.
describeBudgetActionHistories_budgetName :: Lens.Lens' DescribeBudgetActionHistories Core.Text
describeBudgetActionHistories_budgetName = Lens.lens (\DescribeBudgetActionHistories' {budgetName} -> budgetName) (\s@DescribeBudgetActionHistories' {} a -> s {budgetName = a} :: DescribeBudgetActionHistories)

-- | A system-generated universally unique identifier (UUID) for the action.
describeBudgetActionHistories_actionId :: Lens.Lens' DescribeBudgetActionHistories Core.Text
describeBudgetActionHistories_actionId = Lens.lens (\DescribeBudgetActionHistories' {actionId} -> actionId) (\s@DescribeBudgetActionHistories' {} a -> s {actionId = a} :: DescribeBudgetActionHistories)

instance Core.AWSPager DescribeBudgetActionHistories where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeBudgetActionHistoriesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. describeBudgetActionHistoriesResponse_actionHistories
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeBudgetActionHistories_nextToken
          Lens..~ rs
          Lens.^? describeBudgetActionHistoriesResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeBudgetActionHistories
  where
  type
    AWSResponse DescribeBudgetActionHistories =
      DescribeBudgetActionHistoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionHistoriesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "ActionHistories" Core..!@ Core.mempty)
      )

instance Core.Hashable DescribeBudgetActionHistories

instance Core.NFData DescribeBudgetActionHistories

instance Core.ToHeaders DescribeBudgetActionHistories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeBudgetActionHistories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeBudgetActionHistories where
  toJSON DescribeBudgetActionHistories' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("TimePeriod" Core..=) Core.<$> timePeriod,
            Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ActionId" Core..= actionId)
          ]
      )

instance Core.ToPath DescribeBudgetActionHistories where
  toPath = Core.const "/"

instance Core.ToQuery DescribeBudgetActionHistories where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeBudgetActionHistoriesResponse' smart constructor.
data DescribeBudgetActionHistoriesResponse = DescribeBudgetActionHistoriesResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The historical record of the budget action resource.
    actionHistories :: [ActionHistory]
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeBudgetActionHistoriesResponse
newDescribeBudgetActionHistoriesResponse pHttpStatus_ =
  DescribeBudgetActionHistoriesResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      actionHistories = Core.mempty
    }

-- | Undocumented member.
describeBudgetActionHistoriesResponse_nextToken :: Lens.Lens' DescribeBudgetActionHistoriesResponse (Core.Maybe Core.Text)
describeBudgetActionHistoriesResponse_nextToken = Lens.lens (\DescribeBudgetActionHistoriesResponse' {nextToken} -> nextToken) (\s@DescribeBudgetActionHistoriesResponse' {} a -> s {nextToken = a} :: DescribeBudgetActionHistoriesResponse)

-- | The response's http status code.
describeBudgetActionHistoriesResponse_httpStatus :: Lens.Lens' DescribeBudgetActionHistoriesResponse Core.Int
describeBudgetActionHistoriesResponse_httpStatus = Lens.lens (\DescribeBudgetActionHistoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetActionHistoriesResponse' {} a -> s {httpStatus = a} :: DescribeBudgetActionHistoriesResponse)

-- | The historical record of the budget action resource.
describeBudgetActionHistoriesResponse_actionHistories :: Lens.Lens' DescribeBudgetActionHistoriesResponse [ActionHistory]
describeBudgetActionHistoriesResponse_actionHistories = Lens.lens (\DescribeBudgetActionHistoriesResponse' {actionHistories} -> actionHistories) (\s@DescribeBudgetActionHistoriesResponse' {} a -> s {actionHistories = a} :: DescribeBudgetActionHistoriesResponse) Core.. Lens._Coerce

instance
  Core.NFData
    DescribeBudgetActionHistoriesResponse
