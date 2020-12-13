{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.UpdateBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a budget. You can change every part of a budget except for the @budgetName@ and the @calculatedSpend@ . When you modify a budget, the @calculatedSpend@ drops to zero until AWS has new usage data to use for forecasting.
--
-- /Important:/ Only one of @BudgetLimit@ or @PlannedBudgetLimits@ can be present in the syntax at one time. Use the syntax that matches your case. The Request Syntax section shows the @BudgetLimit@ syntax. For @PlannedBudgetLimits@ , see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_UpdateBudget.html#API_UpdateBudget_Examples Examples> section.
module Network.AWS.Budgets.UpdateBudget
  ( -- * Creating a request
    UpdateBudget (..),
    mkUpdateBudget,

    -- ** Request lenses
    ubAccountId,
    ubNewBudget,

    -- * Destructuring the response
    UpdateBudgetResponse (..),
    mkUpdateBudgetResponse,

    -- ** Response lenses
    ubrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of UpdateBudget
--
-- /See:/ 'mkUpdateBudget' smart constructor.
data UpdateBudget = UpdateBudget'
  { -- | The @accountId@ that is associated with the budget that you want to update.
    accountId :: Lude.Text,
    -- | The budget that you want to update your budget to.
    newBudget :: Budget
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBudget' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget that you want to update.
-- * 'newBudget' - The budget that you want to update your budget to.
mkUpdateBudget ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'newBudget'
  Budget ->
  UpdateBudget
mkUpdateBudget pAccountId_ pNewBudget_ =
  UpdateBudget' {accountId = pAccountId_, newBudget = pNewBudget_}

-- | The @accountId@ that is associated with the budget that you want to update.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubAccountId :: Lens.Lens' UpdateBudget Lude.Text
ubAccountId = Lens.lens (accountId :: UpdateBudget -> Lude.Text) (\s a -> s {accountId = a} :: UpdateBudget)
{-# DEPRECATED ubAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The budget that you want to update your budget to.
--
-- /Note:/ Consider using 'newBudget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubNewBudget :: Lens.Lens' UpdateBudget Budget
ubNewBudget = Lens.lens (newBudget :: UpdateBudget -> Budget) (\s a -> s {newBudget = a} :: UpdateBudget)
{-# DEPRECATED ubNewBudget "Use generic-lens or generic-optics with 'newBudget' instead." #-}

instance Lude.AWSRequest UpdateBudget where
  type Rs UpdateBudget = UpdateBudgetResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateBudgetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateBudget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.UpdateBudget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateBudget where
  toJSON UpdateBudget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("NewBudget" Lude..= newBudget)
          ]
      )

instance Lude.ToPath UpdateBudget where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateBudget where
  toQuery = Lude.const Lude.mempty

-- | Response of UpdateBudget
--
-- /See:/ 'mkUpdateBudgetResponse' smart constructor.
newtype UpdateBudgetResponse = UpdateBudgetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateBudgetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateBudgetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateBudgetResponse
mkUpdateBudgetResponse pResponseStatus_ =
  UpdateBudgetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubrsResponseStatus :: Lens.Lens' UpdateBudgetResponse Lude.Int
ubrsResponseStatus = Lens.lens (responseStatus :: UpdateBudgetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateBudgetResponse)
{-# DEPRECATED ubrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
