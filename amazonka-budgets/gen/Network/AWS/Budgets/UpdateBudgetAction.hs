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
-- Module      : Network.AWS.Budgets.UpdateBudgetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget action.
module Network.AWS.Budgets.UpdateBudgetAction
  ( -- * Creating a Request
    UpdateBudgetAction (..),
    newUpdateBudgetAction,

    -- * Request Lenses
    updateBudgetAction_subscribers,
    updateBudgetAction_executionRoleArn,
    updateBudgetAction_approvalModel,
    updateBudgetAction_notificationType,
    updateBudgetAction_actionThreshold,
    updateBudgetAction_definition,
    updateBudgetAction_accountId,
    updateBudgetAction_budgetName,
    updateBudgetAction_actionId,

    -- * Destructuring the Response
    UpdateBudgetActionResponse (..),
    newUpdateBudgetActionResponse,

    -- * Response Lenses
    updateBudgetActionResponse_httpStatus,
    updateBudgetActionResponse_accountId,
    updateBudgetActionResponse_budgetName,
    updateBudgetActionResponse_oldAction,
    updateBudgetActionResponse_newAction,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateBudgetAction' smart constructor.
data UpdateBudgetAction = UpdateBudgetAction'
  { subscribers :: Prelude.Maybe (Prelude.NonEmpty Subscriber),
    -- | The role passed for action execution and reversion. Roles and actions
    -- must be in the same account.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: Prelude.Maybe ApprovalModel,
    notificationType :: Prelude.Maybe NotificationType,
    actionThreshold :: Prelude.Maybe ActionThreshold,
    definition :: Prelude.Maybe Definition,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBudgetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscribers', 'updateBudgetAction_subscribers' - Undocumented member.
--
-- 'executionRoleArn', 'updateBudgetAction_executionRoleArn' - The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
--
-- 'approvalModel', 'updateBudgetAction_approvalModel' - This specifies if the action needs manual or automatic approval.
--
-- 'notificationType', 'updateBudgetAction_notificationType' - Undocumented member.
--
-- 'actionThreshold', 'updateBudgetAction_actionThreshold' - Undocumented member.
--
-- 'definition', 'updateBudgetAction_definition' - Undocumented member.
--
-- 'accountId', 'updateBudgetAction_accountId' - Undocumented member.
--
-- 'budgetName', 'updateBudgetAction_budgetName' - Undocumented member.
--
-- 'actionId', 'updateBudgetAction_actionId' - A system-generated universally unique identifier (UUID) for the action.
newUpdateBudgetAction ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
  UpdateBudgetAction
newUpdateBudgetAction
  pAccountId_
  pBudgetName_
  pActionId_ =
    UpdateBudgetAction'
      { subscribers = Prelude.Nothing,
        executionRoleArn = Prelude.Nothing,
        approvalModel = Prelude.Nothing,
        notificationType = Prelude.Nothing,
        actionThreshold = Prelude.Nothing,
        definition = Prelude.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_
      }

-- | Undocumented member.
updateBudgetAction_subscribers :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe (Prelude.NonEmpty Subscriber))
updateBudgetAction_subscribers = Lens.lens (\UpdateBudgetAction' {subscribers} -> subscribers) (\s@UpdateBudgetAction' {} a -> s {subscribers = a} :: UpdateBudgetAction) Prelude.. Lens.mapping Lens._Coerce

-- | The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
updateBudgetAction_executionRoleArn :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe Prelude.Text)
updateBudgetAction_executionRoleArn = Lens.lens (\UpdateBudgetAction' {executionRoleArn} -> executionRoleArn) (\s@UpdateBudgetAction' {} a -> s {executionRoleArn = a} :: UpdateBudgetAction)

-- | This specifies if the action needs manual or automatic approval.
updateBudgetAction_approvalModel :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe ApprovalModel)
updateBudgetAction_approvalModel = Lens.lens (\UpdateBudgetAction' {approvalModel} -> approvalModel) (\s@UpdateBudgetAction' {} a -> s {approvalModel = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_notificationType :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe NotificationType)
updateBudgetAction_notificationType = Lens.lens (\UpdateBudgetAction' {notificationType} -> notificationType) (\s@UpdateBudgetAction' {} a -> s {notificationType = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_actionThreshold :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe ActionThreshold)
updateBudgetAction_actionThreshold = Lens.lens (\UpdateBudgetAction' {actionThreshold} -> actionThreshold) (\s@UpdateBudgetAction' {} a -> s {actionThreshold = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_definition :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe Definition)
updateBudgetAction_definition = Lens.lens (\UpdateBudgetAction' {definition} -> definition) (\s@UpdateBudgetAction' {} a -> s {definition = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_accountId :: Lens.Lens' UpdateBudgetAction Prelude.Text
updateBudgetAction_accountId = Lens.lens (\UpdateBudgetAction' {accountId} -> accountId) (\s@UpdateBudgetAction' {} a -> s {accountId = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_budgetName :: Lens.Lens' UpdateBudgetAction Prelude.Text
updateBudgetAction_budgetName = Lens.lens (\UpdateBudgetAction' {budgetName} -> budgetName) (\s@UpdateBudgetAction' {} a -> s {budgetName = a} :: UpdateBudgetAction)

-- | A system-generated universally unique identifier (UUID) for the action.
updateBudgetAction_actionId :: Lens.Lens' UpdateBudgetAction Prelude.Text
updateBudgetAction_actionId = Lens.lens (\UpdateBudgetAction' {actionId} -> actionId) (\s@UpdateBudgetAction' {} a -> s {actionId = a} :: UpdateBudgetAction)

instance Core.AWSRequest UpdateBudgetAction where
  type
    AWSResponse UpdateBudgetAction =
      UpdateBudgetActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBudgetActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "AccountId")
            Prelude.<*> (x Core..:> "BudgetName")
            Prelude.<*> (x Core..:> "OldAction")
            Prelude.<*> (x Core..:> "NewAction")
      )

instance Prelude.Hashable UpdateBudgetAction

instance Prelude.NFData UpdateBudgetAction

instance Core.ToHeaders UpdateBudgetAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.UpdateBudgetAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateBudgetAction where
  toJSON UpdateBudgetAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Subscribers" Core..=) Prelude.<$> subscribers,
            ("ExecutionRoleArn" Core..=)
              Prelude.<$> executionRoleArn,
            ("ApprovalModel" Core..=) Prelude.<$> approvalModel,
            ("NotificationType" Core..=)
              Prelude.<$> notificationType,
            ("ActionThreshold" Core..=)
              Prelude.<$> actionThreshold,
            ("Definition" Core..=) Prelude.<$> definition,
            Prelude.Just ("AccountId" Core..= accountId),
            Prelude.Just ("BudgetName" Core..= budgetName),
            Prelude.Just ("ActionId" Core..= actionId)
          ]
      )

instance Core.ToPath UpdateBudgetAction where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateBudgetAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBudgetActionResponse' smart constructor.
data UpdateBudgetActionResponse = UpdateBudgetActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | The previous action resource information.
    oldAction :: Action,
    -- | The updated action resource information.
    newAction' :: Action
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBudgetActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateBudgetActionResponse_httpStatus' - The response's http status code.
--
-- 'accountId', 'updateBudgetActionResponse_accountId' - Undocumented member.
--
-- 'budgetName', 'updateBudgetActionResponse_budgetName' - Undocumented member.
--
-- 'oldAction', 'updateBudgetActionResponse_oldAction' - The previous action resource information.
--
-- 'newAction'', 'updateBudgetActionResponse_newAction' - The updated action resource information.
newUpdateBudgetActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'oldAction'
  Action ->
  -- | 'newAction''
  Action ->
  UpdateBudgetActionResponse
newUpdateBudgetActionResponse
  pHttpStatus_
  pAccountId_
  pBudgetName_
  pOldAction_
  pNewAction_ =
    UpdateBudgetActionResponse'
      { httpStatus =
          pHttpStatus_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        oldAction = pOldAction_,
        newAction' = pNewAction_
      }

-- | The response's http status code.
updateBudgetActionResponse_httpStatus :: Lens.Lens' UpdateBudgetActionResponse Prelude.Int
updateBudgetActionResponse_httpStatus = Lens.lens (\UpdateBudgetActionResponse' {httpStatus} -> httpStatus) (\s@UpdateBudgetActionResponse' {} a -> s {httpStatus = a} :: UpdateBudgetActionResponse)

-- | Undocumented member.
updateBudgetActionResponse_accountId :: Lens.Lens' UpdateBudgetActionResponse Prelude.Text
updateBudgetActionResponse_accountId = Lens.lens (\UpdateBudgetActionResponse' {accountId} -> accountId) (\s@UpdateBudgetActionResponse' {} a -> s {accountId = a} :: UpdateBudgetActionResponse)

-- | Undocumented member.
updateBudgetActionResponse_budgetName :: Lens.Lens' UpdateBudgetActionResponse Prelude.Text
updateBudgetActionResponse_budgetName = Lens.lens (\UpdateBudgetActionResponse' {budgetName} -> budgetName) (\s@UpdateBudgetActionResponse' {} a -> s {budgetName = a} :: UpdateBudgetActionResponse)

-- | The previous action resource information.
updateBudgetActionResponse_oldAction :: Lens.Lens' UpdateBudgetActionResponse Action
updateBudgetActionResponse_oldAction = Lens.lens (\UpdateBudgetActionResponse' {oldAction} -> oldAction) (\s@UpdateBudgetActionResponse' {} a -> s {oldAction = a} :: UpdateBudgetActionResponse)

-- | The updated action resource information.
updateBudgetActionResponse_newAction :: Lens.Lens' UpdateBudgetActionResponse Action
updateBudgetActionResponse_newAction = Lens.lens (\UpdateBudgetActionResponse' {newAction'} -> newAction') (\s@UpdateBudgetActionResponse' {} a -> s {newAction' = a} :: UpdateBudgetActionResponse)

instance Prelude.NFData UpdateBudgetActionResponse
