{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetActionsForBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes all of the budget actions for a budget.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionsForBudget
  ( -- * Creating a request
    DescribeBudgetActionsForBudget (..),
    mkDescribeBudgetActionsForBudget,

    -- ** Request lenses
    dbafbAccountId,
    dbafbBudgetName,
    dbafbMaxResults,
    dbafbNextToken,

    -- * Destructuring the response
    DescribeBudgetActionsForBudgetResponse (..),
    mkDescribeBudgetActionsForBudgetResponse,

    -- ** Response lenses
    dbafbrrsActions,
    dbafbrrsNextToken,
    dbafbrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBudgetActionsForBudget' smart constructor.
data DescribeBudgetActionsForBudget = DescribeBudgetActionsForBudget'
  { accountId :: Types.AccountId,
    budgetName :: Types.BudgetName,
    maxResults :: Core.Maybe Core.Natural,
    nextToken :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetActionsForBudget' value with any optional fields omitted.
mkDescribeBudgetActionsForBudget ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  DescribeBudgetActionsForBudget
mkDescribeBudgetActionsForBudget accountId budgetName =
  DescribeBudgetActionsForBudget'
    { accountId,
      budgetName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbAccountId :: Lens.Lens' DescribeBudgetActionsForBudget Types.AccountId
dbafbAccountId = Lens.field @"accountId"
{-# DEPRECATED dbafbAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbBudgetName :: Lens.Lens' DescribeBudgetActionsForBudget Types.BudgetName
dbafbBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dbafbBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbMaxResults :: Lens.Lens' DescribeBudgetActionsForBudget (Core.Maybe Core.Natural)
dbafbMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dbafbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbNextToken :: Lens.Lens' DescribeBudgetActionsForBudget (Core.Maybe Types.GenericString)
dbafbNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbafbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeBudgetActionsForBudget where
  toJSON DescribeBudgetActionsForBudget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeBudgetActionsForBudget where
  type
    Rs DescribeBudgetActionsForBudget =
      DescribeBudgetActionsForBudgetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSBudgetServiceGateway.DescribeBudgetActionsForBudget"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionsForBudgetResponse'
            Core.<$> (x Core..:? "Actions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeBudgetActionsForBudget where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"actions") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeBudgetActionsForBudgetResponse' smart constructor.
data DescribeBudgetActionsForBudgetResponse = DescribeBudgetActionsForBudgetResponse'
  { -- | A list of the budget action resources information.
    actions :: [Types.Action],
    nextToken :: Core.Maybe Types.GenericString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetActionsForBudgetResponse' value with any optional fields omitted.
mkDescribeBudgetActionsForBudgetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBudgetActionsForBudgetResponse
mkDescribeBudgetActionsForBudgetResponse responseStatus =
  DescribeBudgetActionsForBudgetResponse'
    { actions = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of the budget action resources information.
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrrsActions :: Lens.Lens' DescribeBudgetActionsForBudgetResponse [Types.Action]
dbafbrrsActions = Lens.field @"actions"
{-# DEPRECATED dbafbrrsActions "Use generic-lens or generic-optics with 'actions' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrrsNextToken :: Lens.Lens' DescribeBudgetActionsForBudgetResponse (Core.Maybe Types.GenericString)
dbafbrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbafbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrrsResponseStatus :: Lens.Lens' DescribeBudgetActionsForBudgetResponse Core.Int
dbafbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbafbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
