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
-- Module      : Amazonka.Budgets.DescribeBudgetAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action detail.
module Amazonka.Budgets.DescribeBudgetAction
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

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBudgetAction' smart constructor.
data DescribeBudgetAction = DescribeBudgetAction'
  { accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  -- | 'actionId'
  Prelude.Text ->
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
describeBudgetAction_accountId :: Lens.Lens' DescribeBudgetAction Prelude.Text
describeBudgetAction_accountId = Lens.lens (\DescribeBudgetAction' {accountId} -> accountId) (\s@DescribeBudgetAction' {} a -> s {accountId = a} :: DescribeBudgetAction)

-- | Undocumented member.
describeBudgetAction_budgetName :: Lens.Lens' DescribeBudgetAction Prelude.Text
describeBudgetAction_budgetName = Lens.lens (\DescribeBudgetAction' {budgetName} -> budgetName) (\s@DescribeBudgetAction' {} a -> s {budgetName = a} :: DescribeBudgetAction)

-- | A system-generated universally unique identifier (UUID) for the action.
describeBudgetAction_actionId :: Lens.Lens' DescribeBudgetAction Prelude.Text
describeBudgetAction_actionId = Lens.lens (\DescribeBudgetAction' {actionId} -> actionId) (\s@DescribeBudgetAction' {} a -> s {actionId = a} :: DescribeBudgetAction)

instance Core.AWSRequest DescribeBudgetAction where
  type
    AWSResponse DescribeBudgetAction =
      DescribeBudgetActionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AccountId")
            Prelude.<*> (x Data..:> "BudgetName")
            Prelude.<*> (x Data..:> "Action")
      )

instance Prelude.Hashable DescribeBudgetAction where
  hashWithSalt _salt DescribeBudgetAction' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName
      `Prelude.hashWithSalt` actionId

instance Prelude.NFData DescribeBudgetAction where
  rnf DescribeBudgetAction' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf actionId

instance Data.ToHeaders DescribeBudgetAction where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DescribeBudgetAction" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBudgetAction where
  toJSON DescribeBudgetAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName),
            Prelude.Just ("ActionId" Data..= actionId)
          ]
      )

instance Data.ToPath DescribeBudgetAction where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBudgetAction where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBudgetActionResponse' smart constructor.
data DescribeBudgetActionResponse = DescribeBudgetActionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    accountId :: Prelude.Text,
    budgetName :: Prelude.Text,
    -- | A budget action resource.
    action :: Action
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
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
describeBudgetActionResponse_httpStatus :: Lens.Lens' DescribeBudgetActionResponse Prelude.Int
describeBudgetActionResponse_httpStatus = Lens.lens (\DescribeBudgetActionResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetActionResponse' {} a -> s {httpStatus = a} :: DescribeBudgetActionResponse)

-- | Undocumented member.
describeBudgetActionResponse_accountId :: Lens.Lens' DescribeBudgetActionResponse Prelude.Text
describeBudgetActionResponse_accountId = Lens.lens (\DescribeBudgetActionResponse' {accountId} -> accountId) (\s@DescribeBudgetActionResponse' {} a -> s {accountId = a} :: DescribeBudgetActionResponse)

-- | Undocumented member.
describeBudgetActionResponse_budgetName :: Lens.Lens' DescribeBudgetActionResponse Prelude.Text
describeBudgetActionResponse_budgetName = Lens.lens (\DescribeBudgetActionResponse' {budgetName} -> budgetName) (\s@DescribeBudgetActionResponse' {} a -> s {budgetName = a} :: DescribeBudgetActionResponse)

-- | A budget action resource.
describeBudgetActionResponse_action :: Lens.Lens' DescribeBudgetActionResponse Action
describeBudgetActionResponse_action = Lens.lens (\DescribeBudgetActionResponse' {action} -> action) (\s@DescribeBudgetActionResponse' {} a -> s {action = a} :: DescribeBudgetActionResponse)

instance Prelude.NFData DescribeBudgetActionResponse where
  rnf DescribeBudgetActionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName
      `Prelude.seq` Prelude.rnf action
