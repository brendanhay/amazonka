{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the budgets that are associated with an account.
--
-- /Important:/ The Request Syntax section shows the @BudgetLimit@ syntax. For @PlannedBudgetLimits@ , see the <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_budgets_DescribeBudgets.html#API_DescribeBudgets_Examples Examples> section. 
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgets
    (
    -- * Creating a request
      DescribeBudgets (..)
    , mkDescribeBudgets
    -- ** Request lenses
    , dbAccountId
    , dbMaxResults
    , dbNextToken

    -- * Destructuring the response
    , DescribeBudgetsResponse (..)
    , mkDescribeBudgetsResponse
    -- ** Response lenses
    , dbrrsBudgets
    , dbrrsNextToken
    , dbrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeBudgets 
--
-- /See:/ 'mkDescribeBudgets' smart constructor.
data DescribeBudgets = DescribeBudgets'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budgets that you want descriptions of.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ An optional integer that represents how many entries a paginated response contains. The maximum is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgets' value with any optional fields omitted.
mkDescribeBudgets
    :: Types.AccountId -- ^ 'accountId'
    -> DescribeBudgets
mkDescribeBudgets accountId
  = DescribeBudgets'{accountId, maxResults = Core.Nothing,
                     nextToken = Core.Nothing}

-- | The @accountId@ that is associated with the budgets that you want descriptions of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbAccountId :: Lens.Lens' DescribeBudgets Types.AccountId
dbAccountId = Lens.field @"accountId"
{-# INLINEABLE dbAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbMaxResults :: Lens.Lens' DescribeBudgets (Core.Maybe Core.Natural)
dbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbNextToken :: Lens.Lens' DescribeBudgets (Core.Maybe Types.NextToken)
dbNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeBudgets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBudgets where
        toHeaders DescribeBudgets{..}
          = Core.pure
              ("X-Amz-Target", "AWSBudgetServiceGateway.DescribeBudgets")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBudgets where
        toJSON DescribeBudgets{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeBudgets where
        type Rs DescribeBudgets = DescribeBudgetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBudgetsResponse' Core.<$>
                   (x Core..:? "Budgets") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeBudgets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"budgets" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Response of DescribeBudgets 
--
-- /See:/ 'mkDescribeBudgetsResponse' smart constructor.
data DescribeBudgetsResponse = DescribeBudgetsResponse'
  { budgets :: Core.Maybe [Types.Budget]
    -- ^ A list of budgets.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token in the service response that indicates the next set of results that you can retrieve.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBudgetsResponse' value with any optional fields omitted.
mkDescribeBudgetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBudgetsResponse
mkDescribeBudgetsResponse responseStatus
  = DescribeBudgetsResponse'{budgets = Core.Nothing,
                             nextToken = Core.Nothing, responseStatus}

-- | A list of budgets.
--
-- /Note:/ Consider using 'budgets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsBudgets :: Lens.Lens' DescribeBudgetsResponse (Core.Maybe [Types.Budget])
dbrrsBudgets = Lens.field @"budgets"
{-# INLINEABLE dbrrsBudgets #-}
{-# DEPRECATED budgets "Use generic-lens or generic-optics with 'budgets' instead"  #-}

-- | The pagination token in the service response that indicates the next set of results that you can retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsNextToken :: Lens.Lens' DescribeBudgetsResponse (Core.Maybe Types.NextToken)
dbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbrrsResponseStatus :: Lens.Lens' DescribeBudgetsResponse Core.Int
dbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
