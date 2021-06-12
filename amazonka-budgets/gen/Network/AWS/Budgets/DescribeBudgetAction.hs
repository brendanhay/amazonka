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
-- Module      : Network.AWS.Budgets.DescribeBudgetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action detail.
module Network.AWS.Budgets.DescribeBudgetAction
  ( -- * Creating a Request
    DescribeBudgetAction (..),
    newDescribeBudgetAction,

    -- * Request Lenses
    describeBudgetAction_accountId,
    describeBudgetAction_budgetName,
    describeBudgetAction_actionId,

    -- * Destructuring the Response
    DescribeBudgetActionResponse (..),
    newDescribeBudgetActionResponse,

    -- * Response Lenses
    describeBudgetActionResponse_httpStatus,
    describeBudgetActionResponse_accountId,
    describeBudgetActionResponse_budgetName,
    describeBudgetActionResponse_action,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeBudgetAction' smart constructor.
data DescribeBudgetAction = DescribeBudgetAction'
  { accountId :: Core.Text,
    budgetName :: Core.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBudgetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'describeBudgetAction_accountId' - Undocumented member.
--
-- 'budgetName', 'describeBudgetAction_budgetName' - Undocumented member.
--
-- 'actionId', 'describeBudgetAction_actionId' - A system-generated universally unique identifier (UUID) for the action.
newDescribeBudgetAction ::
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'actionId'
  Core.Text ->
  DescribeBudgetAction
newDescribeBudgetAction
  pAccountId_
  pBudgetName_
  pActionId_ =
    DescribeBudgetAction'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_
      }

-- | Undocumented member.
describeBudgetAction_accountId :: Lens.Lens' DescribeBudgetAction Core.Text
describeBudgetAction_accountId = Lens.lens (\DescribeBudgetAction' {accountId} -> accountId) (\s@DescribeBudgetAction' {} a -> s {accountId = a} :: DescribeBudgetAction)

-- | Undocumented member.
describeBudgetAction_budgetName :: Lens.Lens' DescribeBudgetAction Core.Text
describeBudgetAction_budgetName = Lens.lens (\DescribeBudgetAction' {budgetName} -> budgetName) (\s@DescribeBudgetAction' {} a -> s {budgetName = a} :: DescribeBudgetAction)

-- | A system-generated universally unique identifier (UUID) for the action.
describeBudgetAction_actionId :: Lens.Lens' DescribeBudgetAction Core.Text
describeBudgetAction_actionId = Lens.lens (\DescribeBudgetAction' {actionId} -> actionId) (\s@DescribeBudgetAction' {} a -> s {actionId = a} :: DescribeBudgetAction)

instance Core.AWSRequest DescribeBudgetAction where
  type
    AWSResponse DescribeBudgetAction =
      DescribeBudgetActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "AccountId")
            Core.<*> (x Core..:> "BudgetName")
            Core.<*> (x Core..:> "Action")
      )

instance Core.Hashable DescribeBudgetAction

instance Core.NFData DescribeBudgetAction

instance Core.ToHeaders DescribeBudgetAction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeBudgetAction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeBudgetAction where
  toJSON DescribeBudgetAction' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ActionId" Core..= actionId)
          ]
      )

instance Core.ToPath DescribeBudgetAction where
  toPath = Core.const "/"

instance Core.ToQuery DescribeBudgetAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeBudgetActionResponse' smart constructor.
data DescribeBudgetActionResponse = DescribeBudgetActionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    accountId :: Core.Text,
    budgetName :: Core.Text,
    -- | A budget action resource.
    action :: Action
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeBudgetActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeBudgetActionResponse_httpStatus' - The response's http status code.
--
-- 'accountId', 'describeBudgetActionResponse_accountId' - Undocumented member.
--
-- 'budgetName', 'describeBudgetActionResponse_budgetName' - Undocumented member.
--
-- 'action', 'describeBudgetActionResponse_action' - A budget action resource.
newDescribeBudgetActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'action'
  Action ->
  DescribeBudgetActionResponse
newDescribeBudgetActionResponse
  pHttpStatus_
  pAccountId_
  pBudgetName_
  pAction_ =
    DescribeBudgetActionResponse'
      { httpStatus =
          pHttpStatus_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        action = pAction_
      }

-- | The response's http status code.
describeBudgetActionResponse_httpStatus :: Lens.Lens' DescribeBudgetActionResponse Core.Int
describeBudgetActionResponse_httpStatus = Lens.lens (\DescribeBudgetActionResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetActionResponse' {} a -> s {httpStatus = a} :: DescribeBudgetActionResponse)

-- | Undocumented member.
describeBudgetActionResponse_accountId :: Lens.Lens' DescribeBudgetActionResponse Core.Text
describeBudgetActionResponse_accountId = Lens.lens (\DescribeBudgetActionResponse' {accountId} -> accountId) (\s@DescribeBudgetActionResponse' {} a -> s {accountId = a} :: DescribeBudgetActionResponse)

-- | Undocumented member.
describeBudgetActionResponse_budgetName :: Lens.Lens' DescribeBudgetActionResponse Core.Text
describeBudgetActionResponse_budgetName = Lens.lens (\DescribeBudgetActionResponse' {budgetName} -> budgetName) (\s@DescribeBudgetActionResponse' {} a -> s {budgetName = a} :: DescribeBudgetActionResponse)

-- | A budget action resource.
describeBudgetActionResponse_action :: Lens.Lens' DescribeBudgetActionResponse Action
describeBudgetActionResponse_action = Lens.lens (\DescribeBudgetActionResponse' {action} -> action) (\s@DescribeBudgetActionResponse' {} a -> s {action = a} :: DescribeBudgetActionResponse)

instance Core.NFData DescribeBudgetActionResponse
