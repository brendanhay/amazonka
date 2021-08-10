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
-- Module      : Network.AWS.Budgets.CreateBudgetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a budget action.
module Network.AWS.Budgets.CreateBudgetAction
  ( -- * Creating a Request
    CreateBudgetAction (..),
    newCreateBudgetAction,

    -- * Request Lenses
    createBudgetAction_accountId,
    createBudgetAction_budgetName,
    createBudgetAction_notificationType,
    createBudgetAction_actionType,
    createBudgetAction_actionThreshold,
    createBudgetAction_definition,
    createBudgetAction_executionRoleArn,
    createBudgetAction_approvalModel,
    createBudgetAction_subscribers,

    -- * Destructuring the Response
    CreateBudgetActionResponse (..),
    newCreateBudgetActionResponse,

    -- * Response Lenses
    createBudgetActionResponse_httpStatus,
    createBudgetActionResponse_accountId,
    createBudgetActionResponse_budgetName,
    createBudgetActionResponse_actionId,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBudgetAction' smart constructor.
data CreateBudgetAction = CreateBudgetAction'
  { accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    notificationType :: NotificationType,
    -- | The type of action. This defines the type of tasks that can be carried
    -- out by this action. This field also determines the format for
    -- definition.
    actionType :: ActionType,
    actionThreshold :: ActionThreshold,
    definition :: Definition,
    -- | The role passed for action execution and reversion. Roles and actions
    -- must be in the same account.
    executionRoleArn :: Prelude.Text,
    -- | This specifies if the action needs manual or automatic approval.
    approvalModel :: ApprovalModel,
    subscribers :: Prelude.NonEmpty Subscriber
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBudgetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'createBudgetAction_accountId' - Undocumented member.
--
-- 'budgetName', 'createBudgetAction_budgetName' - Undocumented member.
--
-- 'notificationType', 'createBudgetAction_notificationType' - Undocumented member.
--
-- 'actionType', 'createBudgetAction_actionType' - The type of action. This defines the type of tasks that can be carried
-- out by this action. This field also determines the format for
-- definition.
--
-- 'actionThreshold', 'createBudgetAction_actionThreshold' - Undocumented member.
--
-- 'definition', 'createBudgetAction_definition' - Undocumented member.
--
-- 'executionRoleArn', 'createBudgetAction_executionRoleArn' - The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
--
-- 'approvalModel', 'createBudgetAction_approvalModel' - This specifies if the action needs manual or automatic approval.
--
-- 'subscribers', 'createBudgetAction_subscribers' - Undocumented member.
newCreateBudgetAction ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'notificationType'
  NotificationType ->
  -- | 'actionType'
  ActionType ->
  -- | 'actionThreshold'
  ActionThreshold ->
  -- | 'definition'
  Definition ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'approvalModel'
  ApprovalModel ->
  -- | 'subscribers'
  Prelude.NonEmpty Subscriber ->
  CreateBudgetAction
newCreateBudgetAction
  pAccountId_
  pBudgetName_
  pNotificationType_
  pActionType_
  pActionThreshold_
  pDefinition_
  pExecutionRoleArn_
  pApprovalModel_
  pSubscribers_ =
    CreateBudgetAction'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        notificationType = pNotificationType_,
        actionType = pActionType_,
        actionThreshold = pActionThreshold_,
        definition = pDefinition_,
        executionRoleArn = pExecutionRoleArn_,
        approvalModel = pApprovalModel_,
        subscribers = Lens._Coerce Lens.# pSubscribers_
      }

-- | Undocumented member.
createBudgetAction_accountId :: Lens.Lens' CreateBudgetAction Prelude.Text
createBudgetAction_accountId = Lens.lens (\CreateBudgetAction' {accountId} -> accountId) (\s@CreateBudgetAction' {} a -> s {accountId = a} :: CreateBudgetAction)

-- | Undocumented member.
createBudgetAction_budgetName :: Lens.Lens' CreateBudgetAction Prelude.Text
createBudgetAction_budgetName = Lens.lens (\CreateBudgetAction' {budgetName} -> budgetName) (\s@CreateBudgetAction' {} a -> s {budgetName = a} :: CreateBudgetAction)

-- | Undocumented member.
createBudgetAction_notificationType :: Lens.Lens' CreateBudgetAction NotificationType
createBudgetAction_notificationType = Lens.lens (\CreateBudgetAction' {notificationType} -> notificationType) (\s@CreateBudgetAction' {} a -> s {notificationType = a} :: CreateBudgetAction)

-- | The type of action. This defines the type of tasks that can be carried
-- out by this action. This field also determines the format for
-- definition.
createBudgetAction_actionType :: Lens.Lens' CreateBudgetAction ActionType
createBudgetAction_actionType = Lens.lens (\CreateBudgetAction' {actionType} -> actionType) (\s@CreateBudgetAction' {} a -> s {actionType = a} :: CreateBudgetAction)

-- | Undocumented member.
createBudgetAction_actionThreshold :: Lens.Lens' CreateBudgetAction ActionThreshold
createBudgetAction_actionThreshold = Lens.lens (\CreateBudgetAction' {actionThreshold} -> actionThreshold) (\s@CreateBudgetAction' {} a -> s {actionThreshold = a} :: CreateBudgetAction)

-- | Undocumented member.
createBudgetAction_definition :: Lens.Lens' CreateBudgetAction Definition
createBudgetAction_definition = Lens.lens (\CreateBudgetAction' {definition} -> definition) (\s@CreateBudgetAction' {} a -> s {definition = a} :: CreateBudgetAction)

-- | The role passed for action execution and reversion. Roles and actions
-- must be in the same account.
createBudgetAction_executionRoleArn :: Lens.Lens' CreateBudgetAction Prelude.Text
createBudgetAction_executionRoleArn = Lens.lens (\CreateBudgetAction' {executionRoleArn} -> executionRoleArn) (\s@CreateBudgetAction' {} a -> s {executionRoleArn = a} :: CreateBudgetAction)

-- | This specifies if the action needs manual or automatic approval.
createBudgetAction_approvalModel :: Lens.Lens' CreateBudgetAction ApprovalModel
createBudgetAction_approvalModel = Lens.lens (\CreateBudgetAction' {approvalModel} -> approvalModel) (\s@CreateBudgetAction' {} a -> s {approvalModel = a} :: CreateBudgetAction)

-- | Undocumented member.
createBudgetAction_subscribers :: Lens.Lens' CreateBudgetAction (Prelude.NonEmpty Subscriber)
createBudgetAction_subscribers = Lens.lens (\CreateBudgetAction' {subscribers} -> subscribers) (\s@CreateBudgetAction' {} a -> s {subscribers = a} :: CreateBudgetAction) Prelude.. Lens._Coerce

instance Core.AWSRequest CreateBudgetAction where
  type
    AWSResponse CreateBudgetAction =
      CreateBudgetActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBudgetActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "AccountId")
            Prelude.<*> (x Core..:> "BudgetName")
            Prelude.<*> (x Core..:> "ActionId")
      )

instance Prelude.Hashable CreateBudgetAction

instance Prelude.NFData CreateBudgetAction

instance Core.ToHeaders CreateBudgetAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.CreateBudgetAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBudgetAction where
  toJSON CreateBudgetAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Core..= accountId),
            Prelude.Just ("BudgetName" Core..= budgetName),
            Prelude.Just
              ("NotificationType" Core..= notificationType),
            Prelude.Just ("ActionType" Core..= actionType),
            Prelude.Just
              ("ActionThreshold" Core..= actionThreshold),
            Prelude.Just ("Definition" Core..= definition),
            Prelude.Just
              ("ExecutionRoleArn" Core..= executionRoleArn),
            Prelude.Just ("ApprovalModel" Core..= approvalModel),
            Prelude.Just ("Subscribers" Core..= subscribers)
          ]
      )

instance Core.ToPath CreateBudgetAction where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBudgetAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBudgetActionResponse' smart constructor.
data CreateBudgetActionResponse = CreateBudgetActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBudgetActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBudgetActionResponse_httpStatus' - The response's http status code.
--
-- 'accountId', 'createBudgetActionResponse_accountId' - Undocumented member.
--
-- 'budgetName', 'createBudgetActionResponse_budgetName' - Undocumented member.
--
-- 'actionId', 'createBudgetActionResponse_actionId' - A system-generated universally unique identifier (UUID) for the action.
newCreateBudgetActionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
  CreateBudgetActionResponse
newCreateBudgetActionResponse
  pHttpStatus_
  pAccountId_
  pBudgetName_
  pActionId_ =
    CreateBudgetActionResponse'
      { httpStatus =
          pHttpStatus_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_
      }

-- | The response's http status code.
createBudgetActionResponse_httpStatus :: Lens.Lens' CreateBudgetActionResponse Prelude.Int
createBudgetActionResponse_httpStatus = Lens.lens (\CreateBudgetActionResponse' {httpStatus} -> httpStatus) (\s@CreateBudgetActionResponse' {} a -> s {httpStatus = a} :: CreateBudgetActionResponse)

-- | Undocumented member.
createBudgetActionResponse_accountId :: Lens.Lens' CreateBudgetActionResponse Prelude.Text
createBudgetActionResponse_accountId = Lens.lens (\CreateBudgetActionResponse' {accountId} -> accountId) (\s@CreateBudgetActionResponse' {} a -> s {accountId = a} :: CreateBudgetActionResponse)

-- | Undocumented member.
createBudgetActionResponse_budgetName :: Lens.Lens' CreateBudgetActionResponse Prelude.Text
createBudgetActionResponse_budgetName = Lens.lens (\CreateBudgetActionResponse' {budgetName} -> budgetName) (\s@CreateBudgetActionResponse' {} a -> s {budgetName = a} :: CreateBudgetActionResponse)

-- | A system-generated universally unique identifier (UUID) for the action.
createBudgetActionResponse_actionId :: Lens.Lens' CreateBudgetActionResponse Prelude.Text
createBudgetActionResponse_actionId = Lens.lens (\CreateBudgetActionResponse' {actionId} -> actionId) (\s@CreateBudgetActionResponse' {} a -> s {actionId = a} :: CreateBudgetActionResponse)

instance Prelude.NFData CreateBudgetActionResponse
