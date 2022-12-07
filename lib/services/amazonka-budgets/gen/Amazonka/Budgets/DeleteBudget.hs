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
-- Module      : Amazonka.Budgets.DeleteBudget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget. You can delete your budget at any time.
--
-- Deleting a budget also deletes the notifications and subscribers that
-- are associated with that budget.
module Amazonka.Budgets.DeleteBudget
  ( -- * Creating a Request
    DeleteBudget (..),
    newDeleteBudget,

    -- * Request Lenses
    deleteBudget_accountId,
    deleteBudget_budgetName,

    -- * Destructuring the Response
    DeleteBudgetResponse (..),
    newDeleteBudgetResponse,

    -- * Response Lenses
    deleteBudgetResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of DeleteBudget
--
-- /See:/ 'newDeleteBudget' smart constructor.
data DeleteBudget = DeleteBudget'
  { -- | The @accountId@ that is associated with the budget that you want to
    -- delete.
    accountId :: Prelude.Text,
    -- | The name of the budget that you want to delete.
    budgetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBudget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'deleteBudget_accountId' - The @accountId@ that is associated with the budget that you want to
-- delete.
--
-- 'budgetName', 'deleteBudget_budgetName' - The name of the budget that you want to delete.
newDeleteBudget ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  DeleteBudget
newDeleteBudget pAccountId_ pBudgetName_ =
  DeleteBudget'
    { accountId = pAccountId_,
      budgetName = pBudgetName_
    }

-- | The @accountId@ that is associated with the budget that you want to
-- delete.
deleteBudget_accountId :: Lens.Lens' DeleteBudget Prelude.Text
deleteBudget_accountId = Lens.lens (\DeleteBudget' {accountId} -> accountId) (\s@DeleteBudget' {} a -> s {accountId = a} :: DeleteBudget)

-- | The name of the budget that you want to delete.
deleteBudget_budgetName :: Lens.Lens' DeleteBudget Prelude.Text
deleteBudget_budgetName = Lens.lens (\DeleteBudget' {budgetName} -> budgetName) (\s@DeleteBudget' {} a -> s {budgetName = a} :: DeleteBudget)

instance Core.AWSRequest DeleteBudget where
  type AWSResponse DeleteBudget = DeleteBudgetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBudgetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBudget where
  hashWithSalt _salt DeleteBudget' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName

instance Prelude.NFData DeleteBudget where
  rnf DeleteBudget' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName

instance Data.ToHeaders DeleteBudget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSBudgetServiceGateway.DeleteBudget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBudget where
  toJSON DeleteBudget' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Data..= accountId),
            Prelude.Just ("BudgetName" Data..= budgetName)
          ]
      )

instance Data.ToPath DeleteBudget where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBudget where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DeleteBudget
--
-- /See:/ 'newDeleteBudgetResponse' smart constructor.
data DeleteBudgetResponse = DeleteBudgetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBudgetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteBudgetResponse_httpStatus' - The response's http status code.
newDeleteBudgetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBudgetResponse
newDeleteBudgetResponse pHttpStatus_ =
  DeleteBudgetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteBudgetResponse_httpStatus :: Lens.Lens' DeleteBudgetResponse Prelude.Int
deleteBudgetResponse_httpStatus = Lens.lens (\DeleteBudgetResponse' {httpStatus} -> httpStatus) (\s@DeleteBudgetResponse' {} a -> s {httpStatus = a} :: DeleteBudgetResponse)

instance Prelude.NFData DeleteBudgetResponse where
  rnf DeleteBudgetResponse' {..} =
    Prelude.rnf httpStatus
