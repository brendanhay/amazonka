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
-- Module      : Network.AWS.Budgets.CreateBudget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a budget and, if included, notifications and subscribers.
--
-- Only one of @BudgetLimit@ or @PlannedBudgetLimits@ can be present in the
-- syntax at one time. Use the syntax that matches your case. The Request
-- Syntax section shows the @BudgetLimit@ syntax. For
-- @PlannedBudgetLimits@, see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_CreateBudget.html#API_CreateBudget_Examples Examples>
-- section.
module Network.AWS.Budgets.CreateBudget
  ( -- * Creating a Request
    CreateBudget (..),
    newCreateBudget,

    -- * Request Lenses
    createBudget_notificationsWithSubscribers,
    createBudget_accountId,
    createBudget_budget,

    -- * Destructuring the Response
    CreateBudgetResponse (..),
    newCreateBudgetResponse,

    -- * Response Lenses
    createBudgetResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateBudget
--
-- /See:/ 'newCreateBudget' smart constructor.
data CreateBudget = CreateBudget'
  { -- | A notification that you want to associate with a budget. A budget can
    -- have up to five notifications, and each notification can have one SNS
    -- subscriber and up to 10 email subscribers. If you include notifications
    -- and subscribers in your @CreateBudget@ call, AWS creates the
    -- notifications and subscribers for you.
    notificationsWithSubscribers :: Core.Maybe [NotificationWithSubscribers],
    -- | The @accountId@ that is associated with the budget.
    accountId :: Core.Text,
    -- | The budget object that you want to create.
    budget :: Budget
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBudget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationsWithSubscribers', 'createBudget_notificationsWithSubscribers' - A notification that you want to associate with a budget. A budget can
-- have up to five notifications, and each notification can have one SNS
-- subscriber and up to 10 email subscribers. If you include notifications
-- and subscribers in your @CreateBudget@ call, AWS creates the
-- notifications and subscribers for you.
--
-- 'accountId', 'createBudget_accountId' - The @accountId@ that is associated with the budget.
--
-- 'budget', 'createBudget_budget' - The budget object that you want to create.
newCreateBudget ::
  -- | 'accountId'
  Core.Text ->
  -- | 'budget'
  Budget ->
  CreateBudget
newCreateBudget pAccountId_ pBudget_ =
  CreateBudget'
    { notificationsWithSubscribers =
        Core.Nothing,
      accountId = pAccountId_,
      budget = pBudget_
    }

-- | A notification that you want to associate with a budget. A budget can
-- have up to five notifications, and each notification can have one SNS
-- subscriber and up to 10 email subscribers. If you include notifications
-- and subscribers in your @CreateBudget@ call, AWS creates the
-- notifications and subscribers for you.
createBudget_notificationsWithSubscribers :: Lens.Lens' CreateBudget (Core.Maybe [NotificationWithSubscribers])
createBudget_notificationsWithSubscribers = Lens.lens (\CreateBudget' {notificationsWithSubscribers} -> notificationsWithSubscribers) (\s@CreateBudget' {} a -> s {notificationsWithSubscribers = a} :: CreateBudget) Core.. Lens.mapping Lens._Coerce

-- | The @accountId@ that is associated with the budget.
createBudget_accountId :: Lens.Lens' CreateBudget Core.Text
createBudget_accountId = Lens.lens (\CreateBudget' {accountId} -> accountId) (\s@CreateBudget' {} a -> s {accountId = a} :: CreateBudget)

-- | The budget object that you want to create.
createBudget_budget :: Lens.Lens' CreateBudget Budget
createBudget_budget = Lens.lens (\CreateBudget' {budget} -> budget) (\s@CreateBudget' {} a -> s {budget = a} :: CreateBudget)

instance Core.AWSRequest CreateBudget where
  type AWSResponse CreateBudget = CreateBudgetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateBudgetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateBudget

instance Core.NFData CreateBudget

instance Core.ToHeaders CreateBudget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.CreateBudget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateBudget where
  toJSON CreateBudget' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotificationsWithSubscribers" Core..=)
              Core.<$> notificationsWithSubscribers,
            Core.Just ("AccountId" Core..= accountId),
            Core.Just ("Budget" Core..= budget)
          ]
      )

instance Core.ToPath CreateBudget where
  toPath = Core.const "/"

instance Core.ToQuery CreateBudget where
  toQuery = Core.const Core.mempty

-- | Response of CreateBudget
--
-- /See:/ 'newCreateBudgetResponse' smart constructor.
data CreateBudgetResponse = CreateBudgetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateBudgetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBudgetResponse_httpStatus' - The response's http status code.
newCreateBudgetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateBudgetResponse
newCreateBudgetResponse pHttpStatus_ =
  CreateBudgetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createBudgetResponse_httpStatus :: Lens.Lens' CreateBudgetResponse Core.Int
createBudgetResponse_httpStatus = Lens.lens (\CreateBudgetResponse' {httpStatus} -> httpStatus) (\s@CreateBudgetResponse' {} a -> s {httpStatus = a} :: CreateBudgetResponse)

instance Core.NFData CreateBudgetResponse
