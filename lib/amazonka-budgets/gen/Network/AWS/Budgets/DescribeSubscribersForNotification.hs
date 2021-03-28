{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.DescribeSubscribersForNotification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the subscribers that are associated with a notification.
--
-- This operation returns paginated results.
module Network.AWS.Budgets.DescribeSubscribersForNotification
    (
    -- * Creating a request
      DescribeSubscribersForNotification (..)
    , mkDescribeSubscribersForNotification
    -- ** Request lenses
    , dsfnAccountId
    , dsfnBudgetName
    , dsfnNotification
    , dsfnMaxResults
    , dsfnNextToken

    -- * Destructuring the response
    , DescribeSubscribersForNotificationResponse (..)
    , mkDescribeSubscribersForNotificationResponse
    -- ** Response lenses
    , dsfnrrsNextToken
    , dsfnrrsSubscribers
    , dsfnrrsResponseStatus
    ) where

import qualified Network.AWS.Budgets.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request of DescribeSubscribersForNotification 
--
-- /See:/ 'mkDescribeSubscribersForNotification' smart constructor.
data DescribeSubscribersForNotification = DescribeSubscribersForNotification'
  { accountId :: Types.AccountId
    -- ^ The @accountId@ that is associated with the budget whose subscribers you want descriptions of.
  , budgetName :: Types.BudgetName
    -- ^ The name of the budget whose subscribers you want descriptions of.
  , notification :: Types.Notification
    -- ^ The notification whose subscribers you want to list.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ An optional integer that represents how many entries a paginated response contains. The maximum is 100.
  , nextToken :: Core.Maybe Types.GenericString
    -- ^ The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribersForNotification' value with any optional fields omitted.
mkDescribeSubscribersForNotification
    :: Types.AccountId -- ^ 'accountId'
    -> Types.BudgetName -- ^ 'budgetName'
    -> Types.Notification -- ^ 'notification'
    -> DescribeSubscribersForNotification
mkDescribeSubscribersForNotification accountId budgetName
  notification
  = DescribeSubscribersForNotification'{accountId, budgetName,
                                        notification, maxResults = Core.Nothing,
                                        nextToken = Core.Nothing}

-- | The @accountId@ that is associated with the budget whose subscribers you want descriptions of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnAccountId :: Lens.Lens' DescribeSubscribersForNotification Types.AccountId
dsfnAccountId = Lens.field @"accountId"
{-# INLINEABLE dsfnAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The name of the budget whose subscribers you want descriptions of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnBudgetName :: Lens.Lens' DescribeSubscribersForNotification Types.BudgetName
dsfnBudgetName = Lens.field @"budgetName"
{-# INLINEABLE dsfnBudgetName #-}
{-# DEPRECATED budgetName "Use generic-lens or generic-optics with 'budgetName' instead"  #-}

-- | The notification whose subscribers you want to list.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnNotification :: Lens.Lens' DescribeSubscribersForNotification Types.Notification
dsfnNotification = Lens.field @"notification"
{-# INLINEABLE dsfnNotification #-}
{-# DEPRECATED notification "Use generic-lens or generic-optics with 'notification' instead"  #-}

-- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnMaxResults :: Lens.Lens' DescribeSubscribersForNotification (Core.Maybe Core.Natural)
dsfnMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsfnMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnNextToken :: Lens.Lens' DescribeSubscribersForNotification (Core.Maybe Types.GenericString)
dsfnNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsfnNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeSubscribersForNotification where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSubscribersForNotification where
        toHeaders DescribeSubscribersForNotification{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSBudgetServiceGateway.DescribeSubscribersForNotification")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSubscribersForNotification where
        toJSON DescribeSubscribersForNotification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AccountId" Core..= accountId),
                  Core.Just ("BudgetName" Core..= budgetName),
                  Core.Just ("Notification" Core..= notification),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeSubscribersForNotification where
        type Rs DescribeSubscribersForNotification =
             DescribeSubscribersForNotificationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSubscribersForNotificationResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Subscribers" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeSubscribersForNotification where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"subscribers" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Response of DescribeSubscribersForNotification 
--
-- /See:/ 'mkDescribeSubscribersForNotificationResponse' smart constructor.
data DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token in the service response that indicates the next set of results that you can retrieve.
  , subscribers :: Core.Maybe (Core.NonEmpty Types.Subscriber)
    -- ^ A list of subscribers that are associated with a notification.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribersForNotificationResponse' value with any optional fields omitted.
mkDescribeSubscribersForNotificationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSubscribersForNotificationResponse
mkDescribeSubscribersForNotificationResponse responseStatus
  = DescribeSubscribersForNotificationResponse'{nextToken =
                                                  Core.Nothing,
                                                subscribers = Core.Nothing, responseStatus}

-- | The pagination token in the service response that indicates the next set of results that you can retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrrsNextToken :: Lens.Lens' DescribeSubscribersForNotificationResponse (Core.Maybe Types.NextToken)
dsfnrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsfnrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of subscribers that are associated with a notification.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrrsSubscribers :: Lens.Lens' DescribeSubscribersForNotificationResponse (Core.Maybe (Core.NonEmpty Types.Subscriber))
dsfnrrsSubscribers = Lens.field @"subscribers"
{-# INLINEABLE dsfnrrsSubscribers #-}
{-# DEPRECATED subscribers "Use generic-lens or generic-optics with 'subscribers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrrsResponseStatus :: Lens.Lens' DescribeSubscribersForNotificationResponse Core.Int
dsfnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsfnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
