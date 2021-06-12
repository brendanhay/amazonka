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
-- Module      : Network.AWS.Budgets.ExecuteBudgetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes a budget action.
module Network.AWS.Budgets.ExecuteBudgetAction
  ( -- * Creating a Request
    ExecuteBudgetAction (..),
    newExecuteBudgetAction,

    -- * Request Lenses
    executeBudgetAction_accountId,
    executeBudgetAction_budgetName,
    executeBudgetAction_actionId,
    executeBudgetAction_executionType,

    -- * Destructuring the Response
    ExecuteBudgetActionResponse (..),
    newExecuteBudgetActionResponse,

    -- * Response Lenses
    executeBudgetActionResponse_httpStatus,
    executeBudgetActionResponse_accountId,
    executeBudgetActionResponse_budgetName,
    executeBudgetActionResponse_actionId,
    executeBudgetActionResponse_executionType,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newExecuteBudgetAction' smart constructor.
data ExecuteBudgetAction = ExecuteBudgetAction'
  { accountId :: Core.Text,
    budgetName :: Core.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Core.Text,
    -- | The type of execution.
    executionType :: ExecutionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExecuteBudgetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'executeBudgetAction_accountId' - Undocumented member.
--
-- 'budgetName', 'executeBudgetAction_budgetName' - Undocumented member.
--
-- 'actionId', 'executeBudgetAction_actionId' - A system-generated universally unique identifier (UUID) for the action.
--
-- 'executionType', 'executeBudgetAction_executionType' - The type of execution.
newExecuteBudgetAction ::
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'actionId'
  Core.Text ->
  -- | 'executionType'
  ExecutionType ->
  ExecuteBudgetAction
newExecuteBudgetAction
  pAccountId_
  pBudgetName_
  pActionId_
  pExecutionType_ =
    ExecuteBudgetAction'
      { accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_,
        executionType = pExecutionType_
      }

-- | Undocumented member.
executeBudgetAction_accountId :: Lens.Lens' ExecuteBudgetAction Core.Text
executeBudgetAction_accountId = Lens.lens (\ExecuteBudgetAction' {accountId} -> accountId) (\s@ExecuteBudgetAction' {} a -> s {accountId = a} :: ExecuteBudgetAction)

-- | Undocumented member.
executeBudgetAction_budgetName :: Lens.Lens' ExecuteBudgetAction Core.Text
executeBudgetAction_budgetName = Lens.lens (\ExecuteBudgetAction' {budgetName} -> budgetName) (\s@ExecuteBudgetAction' {} a -> s {budgetName = a} :: ExecuteBudgetAction)

-- | A system-generated universally unique identifier (UUID) for the action.
executeBudgetAction_actionId :: Lens.Lens' ExecuteBudgetAction Core.Text
executeBudgetAction_actionId = Lens.lens (\ExecuteBudgetAction' {actionId} -> actionId) (\s@ExecuteBudgetAction' {} a -> s {actionId = a} :: ExecuteBudgetAction)

-- | The type of execution.
executeBudgetAction_executionType :: Lens.Lens' ExecuteBudgetAction ExecutionType
executeBudgetAction_executionType = Lens.lens (\ExecuteBudgetAction' {executionType} -> executionType) (\s@ExecuteBudgetAction' {} a -> s {executionType = a} :: ExecuteBudgetAction)

instance Core.AWSRequest ExecuteBudgetAction where
  type
    AWSResponse ExecuteBudgetAction =
      ExecuteBudgetActionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteBudgetActionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "AccountId")
            Core.<*> (x Core..:> "BudgetName")
            Core.<*> (x Core..:> "ActionId")
            Core.<*> (x Core..:> "ExecutionType")
      )

instance Core.Hashable ExecuteBudgetAction

instance Core.NFData ExecuteBudgetAction

instance Core.ToHeaders ExecuteBudgetAction where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.ExecuteBudgetAction" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ExecuteBudgetAction where
  toJSON ExecuteBudgetAction' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ActionId" Core..= actionId),
            Core.Just ("ExecutionType" Core..= executionType)
          ]
      )

instance Core.ToPath ExecuteBudgetAction where
  toPath = Core.const "/"

instance Core.ToQuery ExecuteBudgetAction where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newExecuteBudgetActionResponse' smart constructor.
data ExecuteBudgetActionResponse = ExecuteBudgetActionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    accountId :: Core.Text,
    budgetName :: Core.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Core.Text,
    -- | The type of execution.
    executionType :: ExecutionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExecuteBudgetActionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'executeBudgetActionResponse_httpStatus' - The response's http status code.
--
-- 'accountId', 'executeBudgetActionResponse_accountId' - Undocumented member.
--
-- 'budgetName', 'executeBudgetActionResponse_budgetName' - Undocumented member.
--
-- 'actionId', 'executeBudgetActionResponse_actionId' - A system-generated universally unique identifier (UUID) for the action.
--
-- 'executionType', 'executeBudgetActionResponse_executionType' - The type of execution.
newExecuteBudgetActionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'accountId'
  Core.Text ->
  -- | 'budgetName'
  Core.Text ->
  -- | 'actionId'
  Core.Text ->
  -- | 'executionType'
  ExecutionType ->
  ExecuteBudgetActionResponse
newExecuteBudgetActionResponse
  pHttpStatus_
  pAccountId_
  pBudgetName_
  pActionId_
  pExecutionType_ =
    ExecuteBudgetActionResponse'
      { httpStatus =
          pHttpStatus_,
        accountId = pAccountId_,
        budgetName = pBudgetName_,
        actionId = pActionId_,
        executionType = pExecutionType_
      }

-- | The response's http status code.
executeBudgetActionResponse_httpStatus :: Lens.Lens' ExecuteBudgetActionResponse Core.Int
executeBudgetActionResponse_httpStatus = Lens.lens (\ExecuteBudgetActionResponse' {httpStatus} -> httpStatus) (\s@ExecuteBudgetActionResponse' {} a -> s {httpStatus = a} :: ExecuteBudgetActionResponse)

-- | Undocumented member.
executeBudgetActionResponse_accountId :: Lens.Lens' ExecuteBudgetActionResponse Core.Text
executeBudgetActionResponse_accountId = Lens.lens (\ExecuteBudgetActionResponse' {accountId} -> accountId) (\s@ExecuteBudgetActionResponse' {} a -> s {accountId = a} :: ExecuteBudgetActionResponse)

-- | Undocumented member.
executeBudgetActionResponse_budgetName :: Lens.Lens' ExecuteBudgetActionResponse Core.Text
executeBudgetActionResponse_budgetName = Lens.lens (\ExecuteBudgetActionResponse' {budgetName} -> budgetName) (\s@ExecuteBudgetActionResponse' {} a -> s {budgetName = a} :: ExecuteBudgetActionResponse)

-- | A system-generated universally unique identifier (UUID) for the action.
executeBudgetActionResponse_actionId :: Lens.Lens' ExecuteBudgetActionResponse Core.Text
executeBudgetActionResponse_actionId = Lens.lens (\ExecuteBudgetActionResponse' {actionId} -> actionId) (\s@ExecuteBudgetActionResponse' {} a -> s {actionId = a} :: ExecuteBudgetActionResponse)

-- | The type of execution.
executeBudgetActionResponse_executionType :: Lens.Lens' ExecuteBudgetActionResponse ExecutionType
executeBudgetActionResponse_executionType = Lens.lens (\ExecuteBudgetActionResponse' {executionType} -> executionType) (\s@ExecuteBudgetActionResponse' {} a -> s {executionType = a} :: ExecuteBudgetActionResponse)

instance Core.NFData ExecuteBudgetActionResponse
