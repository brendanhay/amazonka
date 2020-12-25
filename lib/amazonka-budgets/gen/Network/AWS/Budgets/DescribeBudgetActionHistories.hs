{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetActionHistories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a budget action history detail.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetActionHistories
  ( -- * Creating a request
    DescribeBudgetActionHistories (..),
    mkDescribeBudgetActionHistories,

    -- ** Request lenses
    dbahAccountId,
    dbahBudgetName,
    dbahActionId,
    dbahMaxResults,
    dbahNextToken,
    dbahTimePeriod,

    -- * Destructuring the response
    DescribeBudgetActionHistoriesResponse (..),
    mkDescribeBudgetActionHistoriesResponse,

    -- ** Response lenses
    dbahrrsActionHistories,
    dbahrrsNextToken,
    dbahrrsResponseStatus,
  )
where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBudgetActionHistories' smart constructor.
data DescribeBudgetActionHistories = DescribeBudgetActionHistories'
  { accountId :: Types.AccountId,
    budgetName :: Types.BudgetName,
    -- | A system-generated universally unique identifier (UUID) for the action.
    actionId :: Types.ActionId,
    maxResults :: Core.Maybe Core.Natural,
    nextToken :: Core.Maybe Types.NextToken,
    timePeriod :: Core.Maybe Types.TimePeriod
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeBudgetActionHistories' value with any optional fields omitted.
mkDescribeBudgetActionHistories ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'actionId'
  Types.ActionId ->
  DescribeBudgetActionHistories
mkDescribeBudgetActionHistories accountId budgetName actionId =
  DescribeBudgetActionHistories'
    { accountId,
      budgetName,
      actionId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      timePeriod = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahAccountId :: Lens.Lens' DescribeBudgetActionHistories Types.AccountId
dbahAccountId = Lens.field @"accountId"
{-# DEPRECATED dbahAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahBudgetName :: Lens.Lens' DescribeBudgetActionHistories Types.BudgetName
dbahBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dbahBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | A system-generated universally unique identifier (UUID) for the action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahActionId :: Lens.Lens' DescribeBudgetActionHistories Types.ActionId
dbahActionId = Lens.field @"actionId"
{-# DEPRECATED dbahActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahMaxResults :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Core.Natural)
dbahMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dbahMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahNextToken :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Types.NextToken)
dbahNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbahNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahTimePeriod :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Types.TimePeriod)
dbahTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED dbahTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

instance Core.FromJSON DescribeBudgetActionHistories where
  toJSON DescribeBudgetActionHistories {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("ActionId" Core..= actionId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("TimePeriod" Core..=) Core.<$> timePeriod
          ]
      )

instance Core.AWSRequest DescribeBudgetActionHistories where
  type
    Rs DescribeBudgetActionHistories =
      DescribeBudgetActionHistoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSBudgetServiceGateway.DescribeBudgetActionHistories"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBudgetActionHistoriesResponse'
            Core.<$> (x Core..:? "ActionHistories" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeBudgetActionHistories where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"actionHistories") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeBudgetActionHistoriesResponse' smart constructor.
data DescribeBudgetActionHistoriesResponse = DescribeBudgetActionHistoriesResponse'
  { -- | The historical record of the budget action resource.
    actionHistories :: [Types.ActionHistory],
    nextToken :: Core.Maybe Types.GenericString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeBudgetActionHistoriesResponse' value with any optional fields omitted.
mkDescribeBudgetActionHistoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBudgetActionHistoriesResponse
mkDescribeBudgetActionHistoriesResponse responseStatus =
  DescribeBudgetActionHistoriesResponse'
    { actionHistories =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The historical record of the budget action resource.
--
-- /Note:/ Consider using 'actionHistories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrrsActionHistories :: Lens.Lens' DescribeBudgetActionHistoriesResponse [Types.ActionHistory]
dbahrrsActionHistories = Lens.field @"actionHistories"
{-# DEPRECATED dbahrrsActionHistories "Use generic-lens or generic-optics with 'actionHistories' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrrsNextToken :: Lens.Lens' DescribeBudgetActionHistoriesResponse (Core.Maybe Types.GenericString)
dbahrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbahrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrrsResponseStatus :: Lens.Lens' DescribeBudgetActionHistoriesResponse Core.Int
dbahrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbahrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
