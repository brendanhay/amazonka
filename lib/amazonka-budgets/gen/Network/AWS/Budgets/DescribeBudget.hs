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
    dbrgrsBudget,
    dbrgrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeBudget
--
-- /See:/ 'mkDescribeBudget' smart constructor.
data DescribeBudget = DescribeBudget'
  { -- | The @accountId@ that is associated with the budget that you want a description of.
    accountId :: Types.AccountId,
    -- | The name of the budget that you want a description of.
    budgetName :: Types.BudgetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudget' value with any optional fields omitted.
mkDescribeBudget ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  DescribeBudget
mkDescribeBudget accountId budgetName =
  DescribeBudget' {accountId, budgetName}

-- | The @accountId@ that is associated with the budget that you want a description of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfAccountId :: Lens.Lens' DescribeBudget Types.AccountId
dbfAccountId = Lens.field @"accountId"
{-# DEPRECATED dbfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget that you want a description of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbfBudgetName :: Lens.Lens' DescribeBudget Types.BudgetName
dbfBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dbfBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

instance Core.FromJSON DescribeBudget where
  toJSON DescribeBudget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName)
          ]
      )

instance Core.AWSRequest DescribeBudget where
  type Rs DescribeBudget = DescribeBudgetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSBudgetServiceGateway.DescribeBudget")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetResponse'
            Core.<$> (x Core..:? "Budget") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Response of DescribeBudget
--
-- /See:/ 'mkDescribeBudgetResponse' smart constructor.
data DescribeBudgetResponse = DescribeBudgetResponse'
  { -- | The description of the budget.
    budget :: Core.Maybe Types.Budget,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeBudgetResponse' value with any optional fields omitted.
mkDescribeBudgetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBudgetResponse
mkDescribeBudgetResponse responseStatus =
  DescribeBudgetResponse' {budget = Core.Nothing, responseStatus}

-- | The description of the budget.
--
-- /Note:/ Consider using 'budget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrgrsBudget :: Lens.Lens' DescribeBudgetResponse (Core.Maybe Types.Budget)
dbrgrsBudget = Lens.field @"budget"
{-# DEPRECATED dbrgrsBudget "Use generic-lens or generic-optics with 'budget' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrgrsResponseStatus :: Lens.Lens' DescribeBudgetResponse Core.Int
dbrgrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
