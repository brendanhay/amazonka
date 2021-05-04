{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Budgets.DeleteBudget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a budget. You can delete your budget at any time.
--
-- Deleting a budget also deletes the notifications and subscribers that
-- are associated with that budget.
module Network.AWS.Budgets.DeleteBudget
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

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteBudget where
  type Rs DeleteBudget = DeleteBudgetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteBudgetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBudget

instance Prelude.NFData DeleteBudget

instance Prelude.ToHeaders DeleteBudget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSBudgetServiceGateway.DeleteBudget" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteBudget where
  toJSON DeleteBudget' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Prelude..= accountId),
            Prelude.Just ("BudgetName" Prelude..= budgetName)
          ]
      )

instance Prelude.ToPath DeleteBudget where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteBudget where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DeleteBudget
--
-- /See:/ 'newDeleteBudgetResponse' smart constructor.
data DeleteBudgetResponse = DeleteBudgetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteBudgetResponse
