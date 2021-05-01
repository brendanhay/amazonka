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
-- Module      : Network.AWS.Budgets.CreateBudget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a budget and, if included, notifications and subscribers.
--
-- Only one of @BudgetLimit@ or @PlannedBudgetLimits@ can be present in the
-- syntax at one time. Use the syntax that matches your case. The Request
-- Syntax section shows the @BudgetLimit@ syntax. For
-- @PlannedBudgetLimits@, see the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_CreateBudget.html#API_CreateBudget_Examples Examples>
-- section.
module Network.AWS.Budgets.CreateBudget
  ( -- * Creating a Request
    CreateBudget (..),
    newCreateBudget,

    -- * Request Lenses
    createBudget_notificationsWithSubscribers,
    createBudget_accountId,
    createBudget_budget,

    -- * Destructuring the Response
    CreateBudgetResponse (..),
    newCreateBudgetResponse,

    -- * Response Lenses
    createBudgetResponse_httpStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of CreateBudget
--
-- /See:/ 'newCreateBudget' smart constructor.
data CreateBudget = CreateBudget'
  { -- | A notification that you want to associate with a budget. A budget can
    -- have up to five notifications, and each notification can have one SNS
    -- subscriber and up to 10 email subscribers. If you include notifications
    -- and subscribers in your @CreateBudget@ call, AWS creates the
    -- notifications and subscribers for you.
    notificationsWithSubscribers :: Prelude.Maybe [NotificationWithSubscribers],
    -- | The @accountId@ that is associated with the budget.
    accountId :: Prelude.Text,
    -- | The budget object that you want to create.
    budget :: Budget
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateBudget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notificationsWithSubscribers', 'createBudget_notificationsWithSubscribers' - A notification that you want to associate with a budget. A budget can
-- have up to five notifications, and each notification can have one SNS
-- subscriber and up to 10 email subscribers. If you include notifications
-- and subscribers in your @CreateBudget@ call, AWS creates the
-- notifications and subscribers for you.
--
-- 'accountId', 'createBudget_accountId' - The @accountId@ that is associated with the budget.
--
-- 'budget', 'createBudget_budget' - The budget object that you want to create.
newCreateBudget ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'budget'
  Budget ->
  CreateBudget
newCreateBudget pAccountId_ pBudget_ =
  CreateBudget'
    { notificationsWithSubscribers =
        Prelude.Nothing,
      accountId = pAccountId_,
      budget = pBudget_
    }

-- | A notification that you want to associate with a budget. A budget can
-- have up to five notifications, and each notification can have one SNS
-- subscriber and up to 10 email subscribers. If you include notifications
-- and subscribers in your @CreateBudget@ call, AWS creates the
-- notifications and subscribers for you.
createBudget_notificationsWithSubscribers :: Lens.Lens' CreateBudget (Prelude.Maybe [NotificationWithSubscribers])
createBudget_notificationsWithSubscribers = Lens.lens (\CreateBudget' {notificationsWithSubscribers} -> notificationsWithSubscribers) (\s@CreateBudget' {} a -> s {notificationsWithSubscribers = a} :: CreateBudget) Prelude.. Lens.mapping Prelude._Coerce

-- | The @accountId@ that is associated with the budget.
createBudget_accountId :: Lens.Lens' CreateBudget Prelude.Text
createBudget_accountId = Lens.lens (\CreateBudget' {accountId} -> accountId) (\s@CreateBudget' {} a -> s {accountId = a} :: CreateBudget)

-- | The budget object that you want to create.
createBudget_budget :: Lens.Lens' CreateBudget Budget
createBudget_budget = Lens.lens (\CreateBudget' {budget} -> budget) (\s@CreateBudget' {} a -> s {budget = a} :: CreateBudget)

instance Prelude.AWSRequest CreateBudget where
  type Rs CreateBudget = CreateBudgetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateBudgetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBudget

instance Prelude.NFData CreateBudget

instance Prelude.ToHeaders CreateBudget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSBudgetServiceGateway.CreateBudget" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateBudget where
  toJSON CreateBudget' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NotificationsWithSubscribers" Prelude..=)
              Prelude.<$> notificationsWithSubscribers,
            Prelude.Just ("AccountId" Prelude..= accountId),
            Prelude.Just ("Budget" Prelude..= budget)
          ]
      )

instance Prelude.ToPath CreateBudget where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateBudget where
  toQuery = Prelude.const Prelude.mempty

-- | Response of CreateBudget
--
-- /See:/ 'newCreateBudgetResponse' smart constructor.
data CreateBudgetResponse = CreateBudgetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateBudgetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createBudgetResponse_httpStatus' - The response's http status code.
newCreateBudgetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBudgetResponse
newCreateBudgetResponse pHttpStatus_ =
  CreateBudgetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createBudgetResponse_httpStatus :: Lens.Lens' CreateBudgetResponse Prelude.Int
createBudgetResponse_httpStatus = Lens.lens (\CreateBudgetResponse' {httpStatus} -> httpStatus) (\s@CreateBudgetResponse' {} a -> s {httpStatus = a} :: CreateBudgetResponse)

instance Prelude.NFData CreateBudgetResponse
