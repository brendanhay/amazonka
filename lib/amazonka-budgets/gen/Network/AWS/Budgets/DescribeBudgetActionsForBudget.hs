{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeBudgetActionsForBudget (..)
    , mkDescribeBudgetActionsForBudget
    -- ** Request lenses
    , dbafbAccountId
    , dbafbBudgetName
    , dbafbMaxResults
    , dbafbNextToken

    -- * Destructuring the response
    , DescribeBudgetActionsForBudgetResponse (..)
    , mkDescribeBudgetActionsForBudgetResponse
    -- ** Response lenses
    , dbafbrrsActions
    , dbafbrrsNextToken
    , dbafbrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBudgetActionsForBudget' smart constructor.
data DescribeBudgetActionsForBudget = DescribeBudgetActionsForBudget'
  { accountId :: Types.AccountId
  , budgetName :: Types.BudgetName
  , maxResults :: Core.Maybe Core.Natural
  , nextToken :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetActionsForBudget' value with any optional fields omitted.
mkDescribeBudgetActionsForBudget
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> DescribeBudgetActionsForBudget
mkDescribeBudgetActionsForBudget accountId budgetName
  = DescribeBudgetActionsForBudget'{accountId, budgetName,
                                    maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbAccountId :: Lens.Lens' DescribeBudgetActionsForBudget Types.AccountId
dbafbAccountId = Lens.field @"accountId"
{-# INLINEABLE dbafbAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbBudgetName :: Lens.Lens' DescribeBudgetActionsForBudget Types.BudgetName
dbafbBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dbafbBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbMaxResults :: Lens.Lens' DescribeBudgetActionsForBudget (Core.Maybe Core.Natural)
dbafbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dbafbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbNextToken :: Lens.Lens' DescribeBudgetActionsForBudget (Core.Maybe Types.GenericString)
dbafbNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbafbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeBudgetActionsForBudget where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeBudgetActionsForBudget where
        toHeaders DescribeBudgetActionsForBudget{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSBudgetServiceGateway.DescribeBudgetActionsForBudget")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeBudgetActionsForBudget where
        toJSON DescribeBudgetActionsForBudget{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeBudgetActionsForBudget where
        type Rs DescribeBudgetActionsForBudget =
             DescribeBudgetActionsForBudgetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeBudgetActionsForBudgetResponse' Core.<$>
                   (x Core..:? "Actions" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeBudgetActionsForBudget where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"actions") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeBudgetActionsForBudgetResponse' smart constructor.
data DescribeBudgetActionsForBudgetResponse = DescribeBudgetActionsForBudgetResponse'
  { actions :: [Types.Action]
    -- ^ A list of the budget action resources information. 
  , nextToken :: Core.Maybe Types.GenericString
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBudgetActionsForBudgetResponse' value with any optional fields omitted.
mkDescribeBudgetActionsForBudgetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeBudgetActionsForBudgetResponse
mkDescribeBudgetActionsForBudgetResponse responseStatus
  = DescribeBudgetActionsForBudgetResponse'{actions = Core.mempty,
                                            nextToken = Core.Nothing, responseStatus}

-- | A list of the budget action resources information. 
--
-- /Note:/ Consider using 'actions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrrsActions :: Lens.Lens' DescribeBudgetActionsForBudgetResponse [Types.Action]
dbafbrrsActions = Lens.field @"actions"
{-# INLINEABLE dbafbrrsActions #-}
{-# DEPRECATED actions "Use generic-lens or generic-optics with 'actions' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrrsNextToken :: Lens.Lens' DescribeBudgetActionsForBudgetResponse (Core.Maybe Types.GenericString)
dbafbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dbafbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbafbrrsResponseStatus :: Lens.Lens' DescribeBudgetActionsForBudgetResponse Core.Int
dbafbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dbafbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
