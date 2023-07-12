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
-- Module      : Amazonka.Budgets.UpdateBudgetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget action.
module Amazonka.Budgets.UpdateBudgetAction
  ( -- * Creating a Request
    UpdateBudgetAction (..),
    newUpdateBudgetAction,

    -- * Request Lenses
    updateBudgetAction_actionThreshold,
    updateBudgetAction_approvalModel,
    updateBudgetAction_definition,
    updateBudgetAction_executionRoleArn,
    updateBudgetAction_notificationType,
    updateBudgetAction_subscribers,
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

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBudgetAction' smart constructor.
data UpdateBudgetAction = UpdateBudgetAction'
  { actionThreshold :: Prelude.Maybe ActionThreshold,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: Prelude.Maybe ApprovalModel,
    definition :: Prelude.Maybe Definition,
    -- | The role passed for action execution and reversion. Roles and actions
    -- must be in the same account.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    notificationType :: Prelude.Maybe NotificationType,
    subscribers :: Prelude.Maybe (Prelude.NonEmpty Subscriber),
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
-- 'actionThreshold', 'updateBudgetAction_actionThreshold' - Undocumented member.
--
-- 'approvalModel', 'updateBudgetAction_approvalModel' - This specifies if the action needs manual or automatic approval.
--
-- 'definition', 'updateBudgetAction_definition' - Undocumented member.
--
-- 'executionRoleArn', 'updateBudgetAction_executionRoleArn' - The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
--
-- 'notificationType', 'updateBudgetAction_notificationType' - Undocumented member.
--
-- 'subscribers', 'updateBudgetAction_subscribers' - Undocumented member.
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
      { actionThreshold =
          Prelude.Nothing,
        approvalModel = Prelude.Nothing,
        definition = Prelude.Nothing,
        executionRoleArn = Prelude.Nothing,
        notificationType = Prelude.Nothing,
        subscribers = Prelude.Nothing,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_
      }

-- | Undocumented member.
updateBudgetAction_actionThreshold :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe ActionThreshold)
updateBudgetAction_actionThreshold = Lens.lens (\UpdateBudgetAction' {actionThreshold} -> actionThreshold) (\s@UpdateBudgetAction' {} a -> s {actionThreshold = a} :: UpdateBudgetAction)

-- | This specifies if the action needs manual or automatic approval.
updateBudgetAction_approvalModel :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe ApprovalModel)
updateBudgetAction_approvalModel = Lens.lens (\UpdateBudgetAction' {approvalModel} -> approvalModel) (\s@UpdateBudgetAction' {} a -> s {approvalModel = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_definition :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe Definition)
updateBudgetAction_definition = Lens.lens (\UpdateBudgetAction' {definition} -> definition) (\s@UpdateBudgetAction' {} a -> s {definition = a} :: UpdateBudgetAction)

-- | The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
updateBudgetAction_executionRoleArn :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe Prelude.Text)
updateBudgetAction_executionRoleArn = Lens.lens (\UpdateBudgetAction' {executionRoleArn} -> executionRoleArn) (\s@UpdateBudgetAction' {} a -> s {executionRoleArn = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_notificationType :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe NotificationType)
updateBudgetAction_notificationType = Lens.lens (\UpdateBudgetAction' {notificationType} -> notificationType) (\s@UpdateBudgetAction' {} a -> s {notificationType = a} :: UpdateBudgetAction)

-- | Undocumented member.
updateBudgetAction_subscribers :: Lens.Lens' UpdateBudgetAction (Prelude.Maybe (Prelude.NonEmpty Subscriber))
updateBudgetAction_subscribers = Lens.lens (\UpdateBudgetAction' {subscribers} -> subscribers) (\s@UpdateBudgetAction' {} a -> s {subscribers = a} :: UpdateBudgetAction) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBudgetActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AccountId")
            Prelude.<*> (x Data..:> "BudgetName")
            Prelude.<*> (x Data..:> "OldAction")
            Prelude.<*> (x Data..:> "NewAction")
      )

instance Prelude.Hashable UpdateBudgetAction where
  hashWithSalt _salt UpdateBudgetAction' {..} =
    _salt
      `Prelude.hashWithSalt` actionThreshold
      `Prelude.hashWithSalt` approvalModel
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` notificationType
      `Prelude.hashWithSalt` subscribers
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` actionId

instance Prelude.NFData UpdateBudgetAction where
  rnf UpdateBudgetAction' {..} =
    Prelude.rnf actionThreshold
      `Prelude.seq` Prelude.rnf approvalModel
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf notificationType
      `Prelude.seq` Prelude.rnf subscribers
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf actionId

instance Data.ToHeaders UpdateBudgetAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.UpdateBudgetAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBudgetAction where
  toJSON UpdateBudgetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActionThreshold" Data..=)
              Prelude.<$> actionThreshold,
            ("ApprovalModel" Data..=) Prelude.<$> approvalModel,
            ("Definition" Data..=) Prelude.<$> definition,
            ("ExecutionRoleArn" Data..=)
              Prelude.<$> executionRoleArn,
            ("NotificationType" Data..=)
              Prelude.<$> notificationType,
            ("Subscribers" Data..=) Prelude.<$> subscribers,
            Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("ActionId" Data..= actionId)
          ]
      )

instance Data.ToPath UpdateBudgetAction where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateBudgetAction where
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

instance Prelude.NFData UpdateBudgetActionResponse where
  rnf UpdateBudgetActionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf oldAction
      `Prelude.seq` Prelude.rnf newAction'
