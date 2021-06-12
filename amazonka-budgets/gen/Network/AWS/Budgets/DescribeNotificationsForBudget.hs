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
-- Module      : Network.AWS.Budgets.DescribeNotificationsForBudget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the notifications that are associated with a budget.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeNotificationsForBudget
  ( -- * Creating a Request
    DescribeNotificationsForBudget (..),
    newDescribeNotificationsForBudget,

    -- * Request Lenses
    describeNotificationsForBudget_nextToken,
    describeNotificationsForBudget_maxResults,
    describeNotificationsForBudget_accountId,
    describeNotificationsForBudget_budgetName,

    -- * Destructuring the Response
    DescribeNotificationsForBudgetResponse (..),
    newDescribeNotificationsForBudgetResponse,

    -- * Response Lenses
    describeNotificationsForBudgetResponse_nextToken,
    describeNotificationsForBudgetResponse_notifications,
    describeNotificationsForBudgetResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeNotificationsForBudget
--
-- /See:/ 'newDescribeNotificationsForBudget' smart constructor.
data DescribeNotificationsForBudget = DescribeNotificationsForBudget'
  { -- | The pagination token that you include in your request to indicate the
    -- next set of results that you want to retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | An optional integer that represents how many entries a paginated
    -- response contains. The maximum is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @accountId@ that is associated with the budget whose notifications
    -- you want descriptions of.
    accountId :: Core.Text,
    -- | The name of the budget whose notifications you want descriptions of.
    budgetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotificationsForBudget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNotificationsForBudget_nextToken' - The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
--
-- 'maxResults', 'describeNotificationsForBudget_maxResults' - An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
--
-- 'accountId', 'describeNotificationsForBudget_accountId' - The @accountId@ that is associated with the budget whose notifications
-- you want descriptions of.
--
-- 'budgetName', 'describeNotificationsForBudget_budgetName' - The name of the budget whose notifications you want descriptions of.
newDescribeNotificationsForBudget ::
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  DescribeNotificationsForBudget
newDescribeNotificationsForBudget
  pAccountId_
  pBudgetName_ =
    DescribeNotificationsForBudget'
      { nextToken =
          Core.Nothing,
        maxResults = Core.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_
      }

-- | The pagination token that you include in your request to indicate the
-- next set of results that you want to retrieve.
describeNotificationsForBudget_nextToken :: Lens.Lens' DescribeNotificationsForBudget (Core.Maybe Core.Text)
describeNotificationsForBudget_nextToken = Lens.lens (\DescribeNotificationsForBudget' {nextToken} -> nextToken) (\s@DescribeNotificationsForBudget' {} a -> s {nextToken = a} :: DescribeNotificationsForBudget)

-- | An optional integer that represents how many entries a paginated
-- response contains. The maximum is 100.
describeNotificationsForBudget_maxResults :: Lens.Lens' DescribeNotificationsForBudget (Core.Maybe Core.Natural)
describeNotificationsForBudget_maxResults = Lens.lens (\DescribeNotificationsForBudget' {maxResults} -> maxResults) (\s@DescribeNotificationsForBudget' {} a -> s {maxResults = a} :: DescribeNotificationsForBudget)

-- | The @accountId@ that is associated with the budget whose notifications
-- you want descriptions of.
describeNotificationsForBudget_accountId :: Lens.Lens' DescribeNotificationsForBudget Core.Text
describeNotificationsForBudget_accountId = Lens.lens (\DescribeNotificationsForBudget' {accountId} -> accountId) (\s@DescribeNotificationsForBudget' {} a -> s {accountId = a} :: DescribeNotificationsForBudget)

-- | The name of the budget whose notifications you want descriptions of.
describeNotificationsForBudget_budgetName :: Lens.Lens' DescribeNotificationsForBudget Core.Text
describeNotificationsForBudget_budgetName = Lens.lens (\DescribeNotificationsForBudget' {budgetName} -> budgetName) (\s@DescribeNotificationsForBudget' {} a -> s {budgetName = a} :: DescribeNotificationsForBudget)

instance Core.AWSPager DescribeNotificationsForBudget where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNotificationsForBudgetResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNotificationsForBudgetResponse_notifications
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeNotificationsForBudget_nextToken
          Lens..~ rs
          Lens.^? describeNotificationsForBudgetResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeNotificationsForBudget
  where
  type
    AWSResponse DescribeNotificationsForBudget =
      DescribeNotificationsForBudgetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotificationsForBudgetResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Notifications" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeNotificationsForBudget

instance Core.NFData DescribeNotificationsForBudget

instance
  Core.ToHeaders
    DescribeNotificationsForBudget
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeNotificationsForBudget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeNotificationsForBudget where
  toJSON DescribeNotificationsForBudget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName)
          ]
      )

instance Core.ToPath DescribeNotificationsForBudget where
  toPath = Core.const "/"

instance Core.ToQuery DescribeNotificationsForBudget where
  toQuery = Core.const Core.mempty

-- | Response of GetNotificationsForBudget
--
-- /See:/ 'newDescribeNotificationsForBudgetResponse' smart constructor.
data DescribeNotificationsForBudgetResponse = DescribeNotificationsForBudgetResponse'
  { -- | The pagination token in the service response that indicates the next set
    -- of results that you can retrieve.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of notifications that are associated with a budget.
    notifications :: Core.Maybe [Notification],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotificationsForBudgetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNotificationsForBudgetResponse_nextToken' - The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
--
-- 'notifications', 'describeNotificationsForBudgetResponse_notifications' - A list of notifications that are associated with a budget.
--
-- 'httpStatus', 'describeNotificationsForBudgetResponse_httpStatus' - The response's http status code.
newDescribeNotificationsForBudgetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNotificationsForBudgetResponse
newDescribeNotificationsForBudgetResponse
  pHttpStatus_ =
    DescribeNotificationsForBudgetResponse'
      { nextToken =
          Core.Nothing,
        notifications = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The pagination token in the service response that indicates the next set
-- of results that you can retrieve.
describeNotificationsForBudgetResponse_nextToken :: Lens.Lens' DescribeNotificationsForBudgetResponse (Core.Maybe Core.Text)
describeNotificationsForBudgetResponse_nextToken = Lens.lens (\DescribeNotificationsForBudgetResponse' {nextToken} -> nextToken) (\s@DescribeNotificationsForBudgetResponse' {} a -> s {nextToken = a} :: DescribeNotificationsForBudgetResponse)

-- | A list of notifications that are associated with a budget.
describeNotificationsForBudgetResponse_notifications :: Lens.Lens' DescribeNotificationsForBudgetResponse (Core.Maybe [Notification])
describeNotificationsForBudgetResponse_notifications = Lens.lens (\DescribeNotificationsForBudgetResponse' {notifications} -> notifications) (\s@DescribeNotificationsForBudgetResponse' {} a -> s {notifications = a} :: DescribeNotificationsForBudgetResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNotificationsForBudgetResponse_httpStatus :: Lens.Lens' DescribeNotificationsForBudgetResponse Core.Int
describeNotificationsForBudgetResponse_httpStatus = Lens.lens (\DescribeNotificationsForBudgetResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationsForBudgetResponse' {} a -> s {httpStatus = a} :: DescribeNotificationsForBudgetResponse)

instance
  Core.NFData
    DescribeNotificationsForBudgetResponse
