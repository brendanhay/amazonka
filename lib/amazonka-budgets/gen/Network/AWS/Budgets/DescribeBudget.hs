{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget.
--
-- /Important:/ The Request Syntax section shows the @BudgetLimit@ syntax. For @PlannedBudgetLimits@ , see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_DescribeBudget.html#API_DescribeBudget_Examples Examples> section.
module Network.AWS.Budgets.DescribeBudget
  ( -- * Creating a request
    DescribeBudget (..),
    mkDescribeBudget,

    -- ** Request lenses
    dbfAccountId,
    dbfBudgetName,

    -- * Destructuring the response
    DescribeBudgetResponse (..),
    mkDescribeBudgetResponse,

    -- ** Response lenses
    dbgrsBudget,
    dbgrsResponseStatus,
  )
where

import Network.AWS.Budgets.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request of DescribeBudget
--
-- /See:/ 'mkDescribeBudget' smart constructor.
data DescribeBudget = DescribeBudget'
  { -- | The @accountId@ that is associated with the budget that you want a description of.
    accountId :: Lude.Text,
    -- | The name of the budget that you want a description of.
    budgetName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudget' with the minimum fields required to make a request.
--
-- * 'accountId' - The @accountId@ that is associated with the budget that you want a description of.
-- * 'budgetName' - The name of the budget that you want a description of.
mkDescribeBudget ::
  -- | 'accountId'
  Lude.Text ->
  -- | 'budgetName'
  Lude.Text ->
  DescribeBudget
mkDescribeBudget pAccountId_ pBudgetName_ =
  DescribeBudget'
    { accountId = pAccountId_,
      budgetName = pBudgetName_
    }

-- | The @accountId@ that is associated with the budget that you want a description of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfAccountId :: Lens.Lens' DescribeBudget Lude.Text
dbfAccountId = Lens.lens (accountId :: DescribeBudget -> Lude.Text) (\s a -> s {accountId = a} :: DescribeBudget)
{-# DEPRECATED dbfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want a description of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfBudgetName :: Lens.Lens' DescribeBudget Lude.Text
dbfBudgetName = Lens.lens (budgetName :: DescribeBudget -> Lude.Text) (\s a -> s {budgetName = a} :: DescribeBudget)
{-# DEPRECATED dbfBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Lude.AWSRequest DescribeBudget where
  type Rs DescribeBudget = DescribeBudgetResponse
  request = Req.postJSON budgetsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBudgetResponse'
            Lude.<$> (x Lude..?> "Budget") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBudget where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSBudgetServiceGateway.DescribeBudget" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBudget where
  toJSON DescribeBudget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AccountId" Lude..= accountId),
            Lude.Just ("BudgetName" Lude..= budgetName)
          ]
      )

instance Lude.ToPath DescribeBudget where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBudget where
  toQuery = Lude.const Lude.mempty

-- | Response of DescribeBudget
--
-- /See:/ 'mkDescribeBudgetResponse' smart constructor.
data DescribeBudgetResponse = DescribeBudgetResponse'
  { -- | The description of the budget.
    budget :: Lude.Maybe Budget,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBudgetResponse' with the minimum fields required to make a request.
--
-- * 'budget' - The description of the budget.
-- * 'responseStatus' - The response status code.
mkDescribeBudgetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBudgetResponse
mkDescribeBudgetResponse pResponseStatus_ =
  DescribeBudgetResponse'
    { budget = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The description of the budget.
--
-- /Note:/ Consider using 'budget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrsBudget :: Lens.Lens' DescribeBudgetResponse (Lude.Maybe Budget)
dbgrsBudget = Lens.lens (budget :: DescribeBudgetResponse -> Lude.Maybe Budget) (\s a -> s {budget = a} :: DescribeBudgetResponse)
{-# DEPRECATED dbgrsBudget "Use generic-lens or generic-optics with 'budget' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbgrsResponseStatus :: Lens.Lens' DescribeBudgetResponse Lude.Int
dbgrsResponseStatus = Lens.lens (responseStatus :: DescribeBudgetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBudgetResponse)
{-# DEPRECATED dbgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
