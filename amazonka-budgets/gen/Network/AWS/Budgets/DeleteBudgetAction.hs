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
-- Module      : Network.AWS.Budgets.DeleteBudgetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget action.
module Network.AWS.Budgets.DeleteBudgetAction
  ( -- * Creating a Request
    DeleteBudgetAction (..),
    newDeleteBudgetAction,

    -- * Request Lenses
    deleteBudgetAction_accountId,
    deleteBudgetAction_budgetName,
    deleteBudgetAction_actionId,

    -- * Destructuring the Response
    DeleteBudgetActionResponse (..),
    newDeleteBudgetActionResponse,

    -- * Response Lenses
    deleteBudgetActionResponse_httpStatus,
    deleteBudgetActionResponse_accountId,
    deleteBudgetActionResponse_budgetName,
    deleteBudgetActionResponse_action,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBudgetAction' smart constructor.
data DeleteBudgetAction = DeleteBudgetAction'
  { accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBudgetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteBudgetAction_accountId' - Undocumented member.
--
-- 'budgetName', 'deleteBudgetAction_budgetName' - Undocumented member.
--
-- 'actionId', 'deleteBudgetAction_actionId' - A system-generated universally unique identifier (UUID) for the action.
newDeleteBudgetAction ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
  DeleteBudgetAction
newDeleteBudgetAction
  pAccountId_
  pBudgetName_
  pActionId_ =
    DeleteBudgetAction'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_
      }

-- | Undocumented member.
deleteBudgetAction_accountId :: Lens.Lens' DeleteBudgetAction Prelude.Text
deleteBudgetAction_accountId = Lens.lens (\DeleteBudgetAction' {accountId} -> accountId) (\s@DeleteBudgetAction' {} a -> s {accountId = a} :: DeleteBudgetAction)

-- | Undocumented member.
deleteBudgetAction_budgetName :: Lens.Lens' DeleteBudgetAction Prelude.Text
deleteBudgetAction_budgetName = Lens.lens (\DeleteBudgetAction' {budgetName} -> budgetName) (\s@DeleteBudgetAction' {} a -> s {budgetName = a} :: DeleteBudgetAction)

-- | A system-generated universally unique identifier (UUID) for the action.
deleteBudgetAction_actionId :: Lens.Lens' DeleteBudgetAction Prelude.Text
deleteBudgetAction_actionId = Lens.lens (\DeleteBudgetAction' {actionId} -> actionId) (\s@DeleteBudgetAction' {} a -> s {actionId = a} :: DeleteBudgetAction)

instance Core.AWSRequest DeleteBudgetAction where
  type
    AWSResponse DeleteBudgetAction =
      DeleteBudgetActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBudgetActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "AccountId")
            Prelude.<*> (x Core..:> "BudgetName")
            Prelude.<*> (x Core..:> "Action")
      )

instance Prelude.Hashable DeleteBudgetAction

instance Prelude.NFData DeleteBudgetAction

instance Core.ToHeaders DeleteBudgetAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DeleteBudgetAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteBudgetAction where
  toJSON DeleteBudgetAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Core..= accountId),
            Prelude.Just ("BudgetName" Core..= budgetName),
            Prelude.Just ("ActionId" Core..= actionId)
          ]
      )

instance Core.ToPath DeleteBudgetAction where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteBudgetAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBudgetActionResponse' smart constructor.
data DeleteBudgetActionResponse = DeleteBudgetActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    action :: Action
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBudgetActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBudgetActionResponse_httpStatus' - The response's http status code.
--
-- 'accountId', 'deleteBudgetActionResponse_accountId' - Undocumented member.
--
-- 'budgetName', 'deleteBudgetActionResponse_budgetName' - Undocumented member.
--
-- 'action', 'deleteBudgetActionResponse_action' - Undocumented member.
newDeleteBudgetActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'action'
  Action ->
  DeleteBudgetActionResponse
newDeleteBudgetActionResponse
  pHttpStatus_
  pAccountId_
  pBudgetName_
  pAction_ =
    DeleteBudgetActionResponse'
      { httpStatus =
          pHttpStatus_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        action = pAction_
      }

-- | The response's http status code.
deleteBudgetActionResponse_httpStatus :: Lens.Lens' DeleteBudgetActionResponse Prelude.Int
deleteBudgetActionResponse_httpStatus = Lens.lens (\DeleteBudgetActionResponse' {httpStatus} -> httpStatus) (\s@DeleteBudgetActionResponse' {} a -> s {httpStatus = a} :: DeleteBudgetActionResponse)

-- | Undocumented member.
deleteBudgetActionResponse_accountId :: Lens.Lens' DeleteBudgetActionResponse Prelude.Text
deleteBudgetActionResponse_accountId = Lens.lens (\DeleteBudgetActionResponse' {accountId} -> accountId) (\s@DeleteBudgetActionResponse' {} a -> s {accountId = a} :: DeleteBudgetActionResponse)

-- | Undocumented member.
deleteBudgetActionResponse_budgetName :: Lens.Lens' DeleteBudgetActionResponse Prelude.Text
deleteBudgetActionResponse_budgetName = Lens.lens (\DeleteBudgetActionResponse' {budgetName} -> budgetName) (\s@DeleteBudgetActionResponse' {} a -> s {budgetName = a} :: DeleteBudgetActionResponse)

-- | Undocumented member.
deleteBudgetActionResponse_action :: Lens.Lens' DeleteBudgetActionResponse Action
deleteBudgetActionResponse_action = Lens.lens (\DeleteBudgetActionResponse' {action} -> action) (\s@DeleteBudgetActionResponse' {} a -> s {action = a} :: DeleteBudgetActionResponse)

instance Prelude.NFData DeleteBudgetActionResponse
