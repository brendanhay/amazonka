{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeSubscribersForNotification (..),
    mkDescribeSubscribersForNotification,

    -- ** Request lenses
    dsfnAccountId,
    dsfnBudgetName,
    dsfnNotification,
    dsfnMaxResults,
    dsfnNextToken,

    -- * Destructuring the response
    DescribeSubscribersForNotificationResponse (..),
    mkDescribeSubscribersForNotificationResponse,

    -- ** Response lenses
    dsfnrrsNextToken,
    dsfnrrsSubscribers,
    dsfnrrsResponseStatus,
  )
where

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
  { -- | The @accountId@ that is associated with the budget whose subscribers you want descriptions of.
    accountId :: Types.AccountId,
    -- | The name of the budget whose subscribers you want descriptions of.
    budgetName :: Types.BudgetName,
    -- | The notification whose subscribers you want to list.
    notification :: Types.Notification,
    -- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
    nextToken :: Core.Maybe Types.GenericString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribersForNotification' value with any optional fields omitted.
mkDescribeSubscribersForNotification ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'budgetName'
  Types.BudgetName ->
  -- | 'notification'
  Types.Notification ->
  DescribeSubscribersForNotification
mkDescribeSubscribersForNotification
  accountId
  budgetName
  notification =
    DescribeSubscribersForNotification'
      { accountId,
        budgetName,
        notification,
        maxResults = Core.Nothing,
        nextToken = Core.Nothing
      }

-- | The @accountId@ that is associated with the budget whose subscribers you want descriptions of.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnAccountId :: Lens.Lens' DescribeSubscribersForNotification Types.AccountId
dsfnAccountId = Lens.field @"accountId"
{-# DEPRECATED dsfnAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The name of the budget whose subscribers you want descriptions of.
--
-- /Note:/ Consider using 'budgetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnBudgetName :: Lens.Lens' DescribeSubscribersForNotification Types.BudgetName
dsfnBudgetName = Lens.field @"budgetName"
{-# DEPRECATED dsfnBudgetName "Use generic-lens or generic-optics with 'budgetName' instead." #-}

-- | The notification whose subscribers you want to list.
--
-- /Note:/ Consider using 'notification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnNotification :: Lens.Lens' DescribeSubscribersForNotification Types.Notification
dsfnNotification = Lens.field @"notification"
{-# DEPRECATED dsfnNotification "Use generic-lens or generic-optics with 'notification' instead." #-}

-- | An optional integer that represents how many entries a paginated response contains. The maximum is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnMaxResults :: Lens.Lens' DescribeSubscribersForNotification (Core.Maybe Core.Natural)
dsfnMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dsfnMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token that you include in your request to indicate the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnNextToken :: Lens.Lens' DescribeSubscribersForNotification (Core.Maybe Types.GenericString)
dsfnNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfnNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON DescribeSubscribersForNotification where
  toJSON DescribeSubscribersForNotification {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccountId" Core..= accountId),
            Core.Just ("BudgetName" Core..= budgetName),
            Core.Just ("Notification" Core..= notification),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest DescribeSubscribersForNotification where
  type
    Rs DescribeSubscribersForNotification =
      DescribeSubscribersForNotificationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSBudgetServiceGateway.DescribeSubscribersForNotification"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSubscribersForNotificationResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Subscribers")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSubscribersForNotification where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"subscribers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Response of DescribeSubscribersForNotification
--
-- /See:/ 'mkDescribeSubscribersForNotificationResponse' smart constructor.
data DescribeSubscribersForNotificationResponse = DescribeSubscribersForNotificationResponse'
  { -- | The pagination token in the service response that indicates the next set of results that you can retrieve.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of subscribers that are associated with a notification.
    subscribers :: Core.Maybe (Core.NonEmpty Types.Subscriber),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSubscribersForNotificationResponse' value with any optional fields omitted.
mkDescribeSubscribersForNotificationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeSubscribersForNotificationResponse
mkDescribeSubscribersForNotificationResponse responseStatus =
  DescribeSubscribersForNotificationResponse'
    { nextToken =
        Core.Nothing,
      subscribers = Core.Nothing,
      responseStatus
    }

-- | The pagination token in the service response that indicates the next set of results that you can retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrrsNextToken :: Lens.Lens' DescribeSubscribersForNotificationResponse (Core.Maybe Types.NextToken)
dsfnrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsfnrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of subscribers that are associated with a notification.
--
-- /Note:/ Consider using 'subscribers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrrsSubscribers :: Lens.Lens' DescribeSubscribersForNotificationResponse (Core.Maybe (Core.NonEmpty Types.Subscriber))
dsfnrrsSubscribers = Lens.field @"subscribers"
{-# DEPRECATED dsfnrrsSubscribers "Use generic-lens or generic-optics with 'subscribers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsfnrrsResponseStatus :: Lens.Lens' DescribeSubscribersForNotificationResponse Core.Int
dsfnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsfnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
