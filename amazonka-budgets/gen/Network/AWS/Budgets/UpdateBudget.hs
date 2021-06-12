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
-- Module      : Network.AWS.Budgets.UpdateBudget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget. You can change every part of a budget except for the
-- @budgetName@ and the @calculatedSpend@. When you modify a budget, the
-- @calculatedSpend@ drops to zero until AWS has new usage data to use for
-- forecasting.
--
-- Only one of @BudgetLimit@ or @PlannedBudgetLimits@ can be present in the
-- syntax at one time. Use the syntax that matches your case. The Request
-- Syntax section shows the @BudgetLimit@ syntax. For
-- @PlannedBudgetLimits@, see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_UpdateBudget.html#API_UpdateBudget_Examples Examples>
-- section.
module Network.AWS.Budgets.UpdateBudget
  ( -- * Creating a Request
    UpdateBudget (..),
    newUpdateBudget,

    -- * Request Lenses
    updateBudget_accountId,
    updateBudget_newBudget,

    -- * Destructuring the Response
    UpdateBudgetResponse (..),
    newUpdateBudgetResponse,

    -- * Response Lenses
    updateBudgetResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of UpdateBudget
--
-- /See:/ 'newUpdateBudget' smart constructor.
data UpdateBudget = UpdateBudget'
  { -- | The @accountId@ that is associated with the budget that you want to
    -- update.
    accountId :: Core.Text,
    -- | The budget that you want to update your budget to.
    newBudget' :: Budget
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBudget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'updateBudget_accountId' - The @accountId@ that is associated with the budget that you want to
-- update.
--
-- 'newBudget'', 'updateBudget_newBudget' - The budget that you want to update your budget to.
newUpdateBudget ::
  -- | 'accountId'
  Core.Text ->
  -- | 'newBudget''
  Budget ->
  UpdateBudget
newUpdateBudget pAccountId_ pNewBudget_ =
  UpdateBudget'
    { accountId = pAccountId_,
      newBudget' = pNewBudget_
    }

-- | The @accountId@ that is associated with the budget that you want to
-- update.
updateBudget_accountId :: Lens.Lens' UpdateBudget Core.Text
updateBudget_accountId = Lens.lens (\UpdateBudget' {accountId} -> accountId) (\s@UpdateBudget' {} a -> s {accountId = a} :: UpdateBudget)

-- | The budget that you want to update your budget to.
updateBudget_newBudget :: Lens.Lens' UpdateBudget Budget
updateBudget_newBudget = Lens.lens (\UpdateBudget' {newBudget'} -> newBudget') (\s@UpdateBudget' {} a -> s {newBudget' = a} :: UpdateBudget)

instance Core.AWSRequest UpdateBudget where
  type AWSResponse UpdateBudget = UpdateBudgetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateBudgetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateBudget

instance Core.NFData UpdateBudget

instance Core.ToHeaders UpdateBudget where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.UpdateBudget" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBudget where
  toJSON UpdateBudget' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("NewBudget" Core..= newBudget')
          ]
      )

instance Core.ToPath UpdateBudget where
  toPath = Core.const "/"

instance Core.ToQuery UpdateBudget where
  toQuery = Core.const Core.mempty

-- | Response of UpdateBudget
--
-- /See:/ 'newUpdateBudgetResponse' smart constructor.
data UpdateBudgetResponse = UpdateBudgetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBudgetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateBudgetResponse_httpStatus' - The response's http status code.
newUpdateBudgetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateBudgetResponse
newUpdateBudgetResponse pHttpStatus_ =
  UpdateBudgetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateBudgetResponse_httpStatus :: Lens.Lens' UpdateBudgetResponse Core.Int
updateBudgetResponse_httpStatus = Lens.lens (\UpdateBudgetResponse' {httpStatus} -> httpStatus) (\s@UpdateBudgetResponse' {} a -> s {httpStatus = a} :: UpdateBudgetResponse)

instance Core.NFData UpdateBudgetResponse
