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
-- Module      : Amazonka.Budgets.DeleteBudgetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget action.
module Amazonka.Budgets.DeleteBudgetAction
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

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBudgetActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AccountId")
            Prelude.<*> (x Data..:> "BudgetName")
            Prelude.<*> (x Data..:> "Action")
      )

instance Prelude.Hashable DeleteBudgetAction where
  hashWithSalt _salt DeleteBudgetAction' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` actionId

instance Prelude.NFData DeleteBudgetAction where
  rnf DeleteBudgetAction' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf actionId

instance Data.ToHeaders DeleteBudgetAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DeleteBudgetAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBudgetAction where
  toJSON DeleteBudgetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("ActionId" Data..= actionId)
          ]
      )

instance Data.ToPath DeleteBudgetAction where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBudgetAction where
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

instance Prelude.NFData DeleteBudgetActionResponse where
  rnf DeleteBudgetActionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf action
