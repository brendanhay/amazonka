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
-- Module      : Amazonka.Budgets.ExecuteBudgetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes a budget action.
module Amazonka.Budgets.ExecuteBudgetAction
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

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExecuteBudgetAction' smart constructor.
data ExecuteBudgetAction = ExecuteBudgetAction'
  { accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text,
    -- | The type of execution.
    executionType :: ExecutionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
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
executeBudgetAction_accountId :: Lens.Lens' ExecuteBudgetAction Prelude.Text
executeBudgetAction_accountId = Lens.lens (\ExecuteBudgetAction' {accountId} -> accountId) (\s@ExecuteBudgetAction' {} a -> s {accountId = a} :: ExecuteBudgetAction)

-- | Undocumented member.
executeBudgetAction_budgetName :: Lens.Lens' ExecuteBudgetAction Prelude.Text
executeBudgetAction_budgetName = Lens.lens (\ExecuteBudgetAction' {budgetName} -> budgetName) (\s@ExecuteBudgetAction' {} a -> s {budgetName = a} :: ExecuteBudgetAction)

-- | A system-generated universally unique identifier (UUID) for the action.
executeBudgetAction_actionId :: Lens.Lens' ExecuteBudgetAction Prelude.Text
executeBudgetAction_actionId = Lens.lens (\ExecuteBudgetAction' {actionId} -> actionId) (\s@ExecuteBudgetAction' {} a -> s {actionId = a} :: ExecuteBudgetAction)

-- | The type of execution.
executeBudgetAction_executionType :: Lens.Lens' ExecuteBudgetAction ExecutionType
executeBudgetAction_executionType = Lens.lens (\ExecuteBudgetAction' {executionType} -> executionType) (\s@ExecuteBudgetAction' {} a -> s {executionType = a} :: ExecuteBudgetAction)

instance Core.AWSRequest ExecuteBudgetAction where
  type
    AWSResponse ExecuteBudgetAction =
      ExecuteBudgetActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExecuteBudgetActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AccountId")
            Prelude.<*> (x Data..:> "BudgetName")
            Prelude.<*> (x Data..:> "ActionId")
            Prelude.<*> (x Data..:> "ExecutionType")
      )

instance Prelude.Hashable ExecuteBudgetAction where
  hashWithSalt _salt ExecuteBudgetAction' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` actionId
      `Prelude.hashWithSalt` executionType

instance Prelude.NFData ExecuteBudgetAction where
  rnf ExecuteBudgetAction' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf budgetName `Prelude.seq`
        Prelude.rnf actionId `Prelude.seq`
          Prelude.rnf executionType

instance Data.ToHeaders ExecuteBudgetAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.ExecuteBudgetAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ExecuteBudgetAction where
  toJSON ExecuteBudgetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("ActionId" Data..= actionId),
            Prelude.Just
              ("ExecutionType" Data..= executionType)
          ]
      )

instance Data.ToPath ExecuteBudgetAction where
  toPath = Prelude.const "/"

instance Data.ToQuery ExecuteBudgetAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExecuteBudgetActionResponse' smart constructor.
data ExecuteBudgetActionResponse = ExecuteBudgetActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text,
    -- | The type of execution.
    executionType :: ExecutionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
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
executeBudgetActionResponse_httpStatus :: Lens.Lens' ExecuteBudgetActionResponse Prelude.Int
executeBudgetActionResponse_httpStatus = Lens.lens (\ExecuteBudgetActionResponse' {httpStatus} -> httpStatus) (\s@ExecuteBudgetActionResponse' {} a -> s {httpStatus = a} :: ExecuteBudgetActionResponse)

-- | Undocumented member.
executeBudgetActionResponse_accountId :: Lens.Lens' ExecuteBudgetActionResponse Prelude.Text
executeBudgetActionResponse_accountId = Lens.lens (\ExecuteBudgetActionResponse' {accountId} -> accountId) (\s@ExecuteBudgetActionResponse' {} a -> s {accountId = a} :: ExecuteBudgetActionResponse)

-- | Undocumented member.
executeBudgetActionResponse_budgetName :: Lens.Lens' ExecuteBudgetActionResponse Prelude.Text
executeBudgetActionResponse_budgetName = Lens.lens (\ExecuteBudgetActionResponse' {budgetName} -> budgetName) (\s@ExecuteBudgetActionResponse' {} a -> s {budgetName = a} :: ExecuteBudgetActionResponse)

-- | A system-generated universally unique identifier (UUID) for the action.
executeBudgetActionResponse_actionId :: Lens.Lens' ExecuteBudgetActionResponse Prelude.Text
executeBudgetActionResponse_actionId = Lens.lens (\ExecuteBudgetActionResponse' {actionId} -> actionId) (\s@ExecuteBudgetActionResponse' {} a -> s {actionId = a} :: ExecuteBudgetActionResponse)

-- | The type of execution.
executeBudgetActionResponse_executionType :: Lens.Lens' ExecuteBudgetActionResponse ExecutionType
executeBudgetActionResponse_executionType = Lens.lens (\ExecuteBudgetActionResponse' {executionType} -> executionType) (\s@ExecuteBudgetActionResponse' {} a -> s {executionType = a} :: ExecuteBudgetActionResponse)

instance Prelude.NFData ExecuteBudgetActionResponse where
  rnf ExecuteBudgetActionResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf accountId `Prelude.seq`
        Prelude.rnf budgetName `Prelude.seq`
          Prelude.rnf actionId `Prelude.seq`
            Prelude.rnf executionType
