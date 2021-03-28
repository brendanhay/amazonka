{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeNotificationsForBudget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the notifications that are associated with a budget.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeNotificationsForBudget
    (
    -- * Creating a request
      DescribeNotificationsForBudget (..)
    , mkDescribeNotificationsForBudget
    -- ** Request lenses
    , dnfbAccountId
    , dnfbBudgetName
    , dnfbMaxResults
    , dnfbNextToken

    -- * Destructuring the response
    , DescribeNotificationsForBudgetResponse (..)
    , mkDescribeNotificationsForBudgetResponse
    -- ** Response lenses
    , dnfbrrsNextToken
    , dnfbrrsNotifications
    , dnfbrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeNotificationsForBudget 
--
-- /See:/ 'mkDescribeNotificationsForBudget' smart constructor.
data DescribeNotificationsForBudget = DescribeNotificationsForBudget'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget whose notifications you want descriptions of.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget whose notifications you want descriptions of.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ An optional integer that represents how many entries a paginated response contains. The maximum is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotificationsForBudget' value with any optional fields omitted.
mkDescribeNotificationsForBudget
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> DescribeNotificationsForBudget
mkDescribeNotificationsForBudget accountId budgetName
  = DescribeNotificationsForBudget'{accountId, budgetName,
                                    maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The @accountId@ that is associated with the budget whose notifications you want descriptions of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbAccountId :: Lens.Lens' DescribeNotificationsForBudget Types.AccountId
dnfbAccountId = Lens.field @"accountId"
{-# INLINEABLE dnfbAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget whose notifications you want descriptions of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbBudgetName :: Lens.Lens' DescribeNotificationsForBudget Types.BudgetName
dnfbBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dnfbBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbMaxResults :: Lens.Lens' DescribeNotificationsForBudget (Core.Maybe Core.Natural)
dnfbMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dnfbMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbNextToken :: Lens.Lens' DescribeNotificationsForBudget (Core.Maybe Types.NextToken)
dnfbNextToken = Lens.field @"nextToken"
{-# INLINEABLE dnfbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeNotificationsForBudget where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeNotificationsForBudget where
        toHeaders DescribeNotificationsForBudget{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSBudgetServiceGateway.DescribeNotificationsForBudget")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeNotificationsForBudget where
        toJSON DescribeNotificationsForBudget{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeNotificationsForBudget where
        type Rs DescribeNotificationsForBudget =
             DescribeNotificationsForBudgetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeNotificationsForBudgetResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Notifications"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeNotificationsForBudget where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"notifications" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Response of GetNotificationsForBudget 
--
-- /See:/ 'mkDescribeNotificationsForBudgetResponse' smart constructor.
data DescribeNotificationsForBudgetResponse = DescribeNotificationsForBudgetResponse'
  { nextToken :: Core.Maybe Types.GenericString
    -- ^ The pagination token in the service response that indicates the next set of results that you can retrieve.
  , notifications :: Core.Maybe [Types.Notification]
    -- ^ A list of notifications that are associated with a budget.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeNotificationsForBudgetResponse' value with any optional fields omitted.
mkDescribeNotificationsForBudgetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeNotificationsForBudgetResponse
mkDescribeNotificationsForBudgetResponse responseStatus
  = DescribeNotificationsForBudgetResponse'{nextToken = Core.Nothing,
                                            notifications = Core.Nothing, responseStatus}

-- | The pagination token in the service response that indicates the next set of results that you can retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbrrsNextToken :: Lens.Lens' DescribeNotificationsForBudgetResponse (Core.Maybe Types.GenericString)
dnfbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dnfbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of notifications that are associated with a budget.
--
-- /Note:/ Consider using 'notifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbrrsNotifications :: Lens.Lens' DescribeNotificationsForBudgetResponse (Core.Maybe [Types.Notification])
dnfbrrsNotifications = Lens.field @"notifications"
{-# INLINEABLE dnfbrrsNotifications #-}
{-# DEPRECATED notifications "Use generic-lens or generic-optics with 'notifications' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfbrrsResponseStatus :: Lens.Lens' DescribeNotificationsForBudgetResponse Core.Int
dnfbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dnfbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
