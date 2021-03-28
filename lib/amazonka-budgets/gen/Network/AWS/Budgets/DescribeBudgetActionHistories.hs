{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeBudgetActionHistories (..)
    , mkDescribeBudgetActionHistories
    -- ** Request lenses
    , dbahAccountId
    , dbahBudgetName
    , dbahActionId
    , dbahMaxResults
    , dbahNextToken
    , dbahTimePeriod

    -- * Destructuring the response
    , DescribeBudgetActionHistoriesResponse (..)
    , mkDescribeBudgetActionHistoriesResponse
    -- ** Response lenses
    , dbahrrsActionHistories
    , dbahrrsNextToken
    , dbahrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBudgetActionHistories' smart constructor.
data DescribeBudgetActionHistories = DescribeBudgetActionHistories'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , actionId :: Types.ActionId
    -- ^ A system-generated universally unique identifier (UUID) for the action. 
  , maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Types.NextToken
  , timePeriod :: Core.Maybe Types.TimePeriod
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBudgetActionHistories' value with any optional fields omitted.
mkDescribeBudgetActionHistories
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.ActionId -- ^ 'actionId'
    -> DescribeBudgetActionHistories
mkDescribeBudgetActionHistories accountId budgetName actionId
  = DescribeBudgetActionHistories'{accountId, budgetName, actionId,
                                   maxResults = Core.Nothing, nextToken = Core.Nothing,
                                   timePeriod = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahAccountId :: Lens.Lens' DescribeBudgetActionHistories Types.AccountId
dbahAccountId = Lens.field @"accountId"
{-# INLINEABLE dbahAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahBudgetName :: Lens.Lens' DescribeBudgetActionHistories Types.BudgetName
dbahBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbahBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | A system-generated universally unique identifier (UUID) for the action. 
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahActionId :: Lens.Lens' DescribeBudgetActionHistories Types.ActionId
dbahActionId = Lens.field @"actionId"
{-# INLINEABLE dbahActionId #-}
{-# DEPRECATED actionId "Use generic-lens or generic-optics with 'actionId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahMaxResults :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Core.Natural)
dbahMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbahMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahNextToken :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Types.NextToken)
dbahNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbahNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahTimePeriod :: Lens.Lens' DescribeBudgetActionHistories (Core.Maybe Types.TimePeriod)
dbahTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE dbahTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

instance Core.ToQuery DescribeBudgetActionHistories where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBudgetActionHistories where
        toHeaders DescribeBudgetActionHistories{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSBudgetServiceGateway.DescribeBudgetActionHistories")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBudgetActionHistories where
        toJSON DescribeBudgetActionHistories{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("ActionId" Core..= actionId),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("TimePeriod" Core..=) Core.<$> timePeriod])

instance Core.AWSRequest DescribeBudgetActionHistories where
        type Rs DescribeBudgetActionHistories =
             DescribeBudgetActionHistoriesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBudgetActionHistoriesResponse' Core.<$>
                   (x Core..:? "ActionHistories" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeBudgetActionHistories where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"actionHistories") =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeBudgetActionHistoriesResponse' smart constructor.
data DescribeBudgetActionHistoriesResponse = DescribeBudgetActionHistoriesResponse'
  { actionHistories :: [Types.ActionHistory]
    -- ^ The historical record of the budget action resource. 
  , nextToken :: Core.Maybe Types.GenericString
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeBudgetActionHistoriesResponse' value with any optional fields omitted.
mkDescribeBudgetActionHistoriesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBudgetActionHistoriesResponse
mkDescribeBudgetActionHistoriesResponse responseStatus
  = DescribeBudgetActionHistoriesResponse'{actionHistories =
                                             Core.mempty,
                                           nextToken = Core.Nothing, responseStatus}

-- | The historical record of the budget action resource. 
--
-- /Note:/ Consider using 'actionHistories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrrsActionHistories :: Lens.Lens' DescribeBudgetActionHistoriesResponse [Types.ActionHistory]
dbahrrsActionHistories = Lens.field @"actionHistories"
{-# INLINEABLE dbahrrsActionHistories #-}
{-# DEPRECATED actionHistories "Use generic-lens or generic-optics with 'actionHistories' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrrsNextToken :: Lens.Lens' DescribeBudgetActionHistoriesResponse (Core.Maybe Types.GenericString)
dbahrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbahrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbahrrsResponseStatus :: Lens.Lens' DescribeBudgetActionHistoriesResponse Core.Int
dbahrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbahrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
