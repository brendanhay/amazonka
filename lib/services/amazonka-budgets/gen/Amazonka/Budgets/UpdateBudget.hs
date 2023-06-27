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
-- Module      : Amazonka.Budgets.UpdateBudget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget. You can change every part of a budget except for the
-- @budgetName@ and the @calculatedSpend@. When you modify a budget, the
-- @calculatedSpend@ drops to zero until Amazon Web Services has new usage
-- data to use for forecasting.
--
-- Only one of @BudgetLimit@ or @PlannedBudgetLimits@ can be present in the
-- syntax at one time. Use the syntax that matches your case. The Request
-- Syntax section shows the @BudgetLimit@ syntax. For
-- @PlannedBudgetLimits@, see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_UpdateBudget.html#API_UpdateBudget_Examples Examples>
-- section.
module Amazonka.Budgets.UpdateBudget
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

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of UpdateBudget
--
-- /See:/ 'newUpdateBudget' smart constructor.
data UpdateBudget = UpdateBudget'
  { -- | The @accountId@ that is associated with the budget that you want to
    -- update.
    accountId :: Prelude.Text,
    -- | The budget that you want to update your budget to.
    newBudget' :: Budget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
updateBudget_accountId :: Lens.Lens' UpdateBudget Prelude.Text
updateBudget_accountId = Lens.lens (\UpdateBudget' {accountId} -> accountId) (\s@UpdateBudget' {} a -> s {accountId = a} :: UpdateBudget)

-- | The budget that you want to update your budget to.
updateBudget_newBudget :: Lens.Lens' UpdateBudget Budget
updateBudget_newBudget = Lens.lens (\UpdateBudget' {newBudget'} -> newBudget') (\s@UpdateBudget' {} a -> s {newBudget' = a} :: UpdateBudget)

instance Core.AWSRequest UpdateBudget where
  type AWSResponse UpdateBudget = UpdateBudgetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateBudgetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBudget where
  hashWithSalt _salt UpdateBudget' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` newBudget'

instance Prelude.NFData UpdateBudget where
  rnf UpdateBudget' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf newBudget'

instance Data.ToHeaders UpdateBudget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.UpdateBudget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBudget where
  toJSON UpdateBudget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("NewBudget" Data..= newBudget')
          ]
      )

instance Data.ToPath UpdateBudget where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateBudget where
  toQuery = Prelude.const Prelude.mempty

-- | Response of UpdateBudget
--
-- /See:/ 'newUpdateBudgetResponse' smart constructor.
data UpdateBudgetResponse = UpdateBudgetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateBudgetResponse
newUpdateBudgetResponse pHttpStatus_ =
  UpdateBudgetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateBudgetResponse_httpStatus :: Lens.Lens' UpdateBudgetResponse Prelude.Int
updateBudgetResponse_httpStatus = Lens.lens (\UpdateBudgetResponse' {httpStatus} -> httpStatus) (\s@UpdateBudgetResponse' {} a -> s {httpStatus = a} :: UpdateBudgetResponse)

instance Prelude.NFData UpdateBudgetResponse where
  rnf UpdateBudgetResponse' {..} =
    Prelude.rnf httpStatus
