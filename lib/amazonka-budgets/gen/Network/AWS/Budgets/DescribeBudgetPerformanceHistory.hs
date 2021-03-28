{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeBudgetPerformanceHistory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the history for @DAILY@ , @MONTHLY@ , and @QUARTERLY@ budgets. Budget history isn't available for @ANNUAL@ budgets.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeBudgetPerformanceHistory
    (
    -- * Creating a request
      DescribeBudgetPerformanceHistory (..)
    , mkDescribeBudgetPerformanceHistory
    -- ** Request lenses
    , dbphAccountId
    , dbphBudgetName
    , dbphMaxResults
    , dbphNextToken
    , dbphTimePeriod

    -- * Destructuring the response
    , DescribeBudgetPerformanceHistoryResponse (..)
    , mkDescribeBudgetPerformanceHistoryResponse
    -- ** Response lenses
    , dbphrrsBudgetPerformanceHistory
    , dbphrrsNextToken
    , dbphrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBudgetPerformanceHistory' smart constructor.
data DescribeBudgetPerformanceHistory = DescribeBudgetPerformanceHistory'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Types.NextToken
  , timePeriod :: Core.Maybe Types.TimePeriod
    -- ^ Retrieves how often the budget went into an @ALARM@ state for the specified time period.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBudgetPerformanceHistory' value with any optional fields omitted.
mkDescribeBudgetPerformanceHistory
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> DescribeBudgetPerformanceHistory
mkDescribeBudgetPerformanceHistory accountId budgetName
  = DescribeBudgetPerformanceHistory'{accountId, budgetName,
                                      maxResults = Core.Nothing, nextToken = Core.Nothing,
                                      timePeriod = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphAccountId :: Lens.Lens' DescribeBudgetPerformanceHistory Types.AccountId
dbphAccountId = Lens.field @"accountId"
{-# INLINEABLE dbphAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphBudgetName :: Lens.Lens' DescribeBudgetPerformanceHistory Types.BudgetName
dbphBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbphBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphMaxResults :: Lens.Lens' DescribeBudgetPerformanceHistory (Core.Maybe Core.Natural)
dbphMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbphMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphNextToken :: Lens.Lens' DescribeBudgetPerformanceHistory (Core.Maybe Types.NextToken)
dbphNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbphNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Retrieves how often the budget went into an @ALARM@ state for the specified time period.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphTimePeriod :: Lens.Lens' DescribeBudgetPerformanceHistory (Core.Maybe Types.TimePeriod)
dbphTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE dbphTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

instance Core.ToQuery DescribeBudgetPerformanceHistory where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBudgetPerformanceHistory where
        toHeaders DescribeBudgetPerformanceHistory{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSBudgetServiceGateway.DescribeBudgetPerformanceHistory")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBudgetPerformanceHistory where
        toJSON DescribeBudgetPerformanceHistory{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("TimePeriod" Core..=) Core.<$> timePeriod])

instance Core.AWSRequest DescribeBudgetPerformanceHistory where
        type Rs DescribeBudgetPerformanceHistory =
             DescribeBudgetPerformanceHistoryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBudgetPerformanceHistoryResponse' Core.<$>
                   (x Core..:? "BudgetPerformanceHistory") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeBudgetPerformanceHistory where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"budgetPerformanceHistory" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeBudgetPerformanceHistoryResponse' smart constructor.
data DescribeBudgetPerformanceHistoryResponse = DescribeBudgetPerformanceHistoryResponse'
  { budgetPerformanceHistory :: Core.Maybe Types.BudgetPerformanceHistory
    -- ^ The history of how often the budget has gone into an @ALARM@ state.
--
-- For @DAILY@ budgets, the history saves the state of the budget for the last 60 days. For @MONTHLY@ budgets, the history saves the state of the budget for the current month plus the last 12 months. For @QUARTERLY@ budgets, the history saves the state of the budget for the last four quarters.
  , nextToken :: Core.Maybe Types.GenericString
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBudgetPerformanceHistoryResponse' value with any optional fields omitted.
mkDescribeBudgetPerformanceHistoryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBudgetPerformanceHistoryResponse
mkDescribeBudgetPerformanceHistoryResponse responseStatus
  = DescribeBudgetPerformanceHistoryResponse'{budgetPerformanceHistory
                                                = Core.Nothing,
                                              nextToken = Core.Nothing, responseStatus}

-- | The history of how often the budget has gone into an @ALARM@ state.
--
-- For @DAILY@ budgets, the history saves the state of the budget for the last 60 days. For @MONTHLY@ budgets, the history saves the state of the budget for the current month plus the last 12 months. For @QUARTERLY@ budgets, the history saves the state of the budget for the last four quarters.
--
-- /Note:/ Consider using 'budgetPerformanceHistory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphrrsBudgetPerformanceHistory :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse (Core.Maybe Types.BudgetPerformanceHistory)
dbphrrsBudgetPerformanceHistory = Lens.field @"budgetPerformanceHistory"
{-# INLINEABLE dbphrrsBudgetPerformanceHistory #-}
{-# DEPRECATED budgetPerformanceHistory "Use generic-lens or generic-optics with 'budgetPerformanceHistory' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphrrsNextToken :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse (Core.Maybe Types.GenericString)
dbphrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbphrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbphrrsResponseStatus :: Lens.Lens' DescribeBudgetPerformanceHistoryResponse Core.Int
dbphrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbphrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
