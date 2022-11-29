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
-- Module      : Amazonka.Budgets.DescribeBudget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget.
--
-- The Request Syntax section shows the @BudgetLimit@ syntax. For
-- @PlannedBudgetLimits@, see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_DescribeBudget.html#API_DescribeBudget_Examples Examples>
-- section.
module Amazonka.Budgets.DescribeBudget
  ( -- * Creating a Request
    DescribeBudget (..),
    newDescribeBudget,

    -- * Request Lenses
    describeBudget_accountId,
    describeBudget_budgetName,

    -- * Destructuring the Response
    DescribeBudgetResponse (..),
    newDescribeBudgetResponse,

    -- * Response Lenses
    describeBudgetResponse_budget,
    describeBudgetResponse_httpStatus,
  )
where

import Amazonka.Budgets.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request of DescribeBudget
--
-- /See:/ 'newDescribeBudget' smart constructor.
data DescribeBudget = DescribeBudget'
  { -- | The @accountId@ that is associated with the budget that you want a
    -- description of.
    accountId :: Prelude.Text,
    -- | The name of the budget that you want a description of.
    budgetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'describeBudget_accountId' - The @accountId@ that is associated with the budget that you want a
-- description of.
--
-- 'budgetName', 'describeBudget_budgetName' - The name of the budget that you want a description of.
newDescribeBudget ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budgetName'
  Prelude.Text ->
  DescribeBudget
newDescribeBudget pAccountId_ pBudgetName_ =
  DescribeBudget'
    { accountId = pAccountId_,
      budgetName = pBudgetName_
    }

-- | The @accountId@ that is associated with the budget that you want a
-- description of.
describeBudget_accountId :: Lens.Lens' DescribeBudget Prelude.Text
describeBudget_accountId = Lens.lens (\DescribeBudget' {accountId} -> accountId) (\s@DescribeBudget' {} a -> s {accountId = a} :: DescribeBudget)

-- | The name of the budget that you want a description of.
describeBudget_budgetName :: Lens.Lens' DescribeBudget Prelude.Text
describeBudget_budgetName = Lens.lens (\DescribeBudget' {budgetName} -> budgetName) (\s@DescribeBudget' {} a -> s {budgetName = a} :: DescribeBudget)

instance Core.AWSRequest DescribeBudget where
  type
    AWSResponse DescribeBudget =
      DescribeBudgetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetResponse'
            Prelude.<$> (x Core..?> "Budget")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBudget where
  hashWithSalt _salt DescribeBudget' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` budgetName

instance Prelude.NFData DescribeBudget where
  rnf DescribeBudget' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf budgetName

instance Core.ToHeaders DescribeBudget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSBudgetServiceGateway.DescribeBudget" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeBudget where
  toJSON DescribeBudget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("AccountId" Core..= accountId),
            Prelude.Just ("BudgetName" Core..= budgetName)
          ]
      )

instance Core.ToPath DescribeBudget where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeBudget where
  toQuery = Prelude.const Prelude.mempty

-- | Response of DescribeBudget
--
-- /See:/ 'newDescribeBudgetResponse' smart constructor.
data DescribeBudgetResponse = DescribeBudgetResponse'
  { -- | The description of the budget.
    budget :: Prelude.Maybe Budget,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBudgetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'budget', 'describeBudgetResponse_budget' - The description of the budget.
--
-- 'httpStatus', 'describeBudgetResponse_httpStatus' - The response's http status code.
newDescribeBudgetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBudgetResponse
newDescribeBudgetResponse pHttpStatus_ =
  DescribeBudgetResponse'
    { budget = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the budget.
describeBudgetResponse_budget :: Lens.Lens' DescribeBudgetResponse (Prelude.Maybe Budget)
describeBudgetResponse_budget = Lens.lens (\DescribeBudgetResponse' {budget} -> budget) (\s@DescribeBudgetResponse' {} a -> s {budget = a} :: DescribeBudgetResponse)

-- | The response's http status code.
describeBudgetResponse_httpStatus :: Lens.Lens' DescribeBudgetResponse Prelude.Int
describeBudgetResponse_httpStatus = Lens.lens (\DescribeBudgetResponse' {httpStatus} -> httpStatus) (\s@DescribeBudgetResponse' {} a -> s {httpStatus = a} :: DescribeBudgetResponse)

instance Prelude.NFData DescribeBudgetResponse where
  rnf DescribeBudgetResponse' {..} =
    Prelude.rnf budget
      `Prelude.seq` Prelude.rnf httpStatus
