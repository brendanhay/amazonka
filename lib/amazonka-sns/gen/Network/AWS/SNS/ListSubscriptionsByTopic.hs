{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the subscriptions to a specific topic. Each call returns a limited list of subscriptions, up to 100. If there are more subscriptions, a @NextToken@ is also returned. Use the @NextToken@ parameter in a new @ListSubscriptionsByTopic@ call to get further results.
--
-- This action is throttled at 30 transactions per second (TPS).
--
-- This operation returns paginated results.
module Network.AWS.SNS.ListSubscriptionsByTopic
  ( -- * Creating a request
    ListSubscriptionsByTopic (..),
    mkListSubscriptionsByTopic,

    -- ** Request lenses
    lsbtTopicArn,
    lsbtNextToken,

    -- * Destructuring the response
    ListSubscriptionsByTopicResponse (..),
    mkListSubscriptionsByTopicResponse,

    -- ** Response lenses
    lsbtrrsNextToken,
    lsbtrrsSubscriptions,
    lsbtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SNS.Types as Types

-- | Input for ListSubscriptionsByTopic action.
--
-- /See:/ 'mkListSubscriptionsByTopic' smart constructor.
data ListSubscriptionsByTopic = ListSubscriptionsByTopic'
  { -- | The ARN of the topic for which you wish to find subscriptions.
    topicArn :: Types.TopicArn,
    -- | Token returned by the previous @ListSubscriptionsByTopic@ request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionsByTopic' value with any optional fields omitted.
mkListSubscriptionsByTopic ::
  -- | 'topicArn'
  Types.TopicArn ->
  ListSubscriptionsByTopic
mkListSubscriptionsByTopic topicArn =
  ListSubscriptionsByTopic' {topicArn, nextToken = Core.Nothing}

-- | The ARN of the topic for which you wish to find subscriptions.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtTopicArn :: Lens.Lens' ListSubscriptionsByTopic Types.TopicArn
lsbtTopicArn = Lens.field @"topicArn"
{-# DEPRECATED lsbtTopicArn "Use generic-lens or generic-optics with 'topicArn' instead." #-}

-- | Token returned by the previous @ListSubscriptionsByTopic@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtNextToken :: Lens.Lens' ListSubscriptionsByTopic (Core.Maybe Types.NextToken)
lsbtNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsbtNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListSubscriptionsByTopic where
  type Rs ListSubscriptionsByTopic = ListSubscriptionsByTopicResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListSubscriptionsByTopic")
                Core.<> (Core.pure ("Version", "2010-03-31"))
                Core.<> (Core.toQueryValue "TopicArn" topicArn)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListSubscriptionsByTopicResult"
      ( \s h x ->
          ListSubscriptionsByTopicResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "Subscriptions" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSubscriptionsByTopic where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"subscriptions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Response for ListSubscriptionsByTopic action.
--
-- /See:/ 'mkListSubscriptionsByTopicResponse' smart constructor.
data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse'
  { -- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This element is returned if there are more subscriptions to retrieve.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of subscriptions.
    subscriptions :: Core.Maybe [Types.Subscription],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSubscriptionsByTopicResponse' value with any optional fields omitted.
mkListSubscriptionsByTopicResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSubscriptionsByTopicResponse
mkListSubscriptionsByTopicResponse responseStatus =
  ListSubscriptionsByTopicResponse'
    { nextToken = Core.Nothing,
      subscriptions = Core.Nothing,
      responseStatus
    }

-- | Token to pass along to the next @ListSubscriptionsByTopic@ request. This element is returned if there are more subscriptions to retrieve.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtrrsNextToken :: Lens.Lens' ListSubscriptionsByTopicResponse (Core.Maybe Types.NextToken)
lsbtrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsbtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtrrsSubscriptions :: Lens.Lens' ListSubscriptionsByTopicResponse (Core.Maybe [Types.Subscription])
lsbtrrsSubscriptions = Lens.field @"subscriptions"
{-# DEPRECATED lsbtrrsSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsbtrrsResponseStatus :: Lens.Lens' ListSubscriptionsByTopicResponse Core.Int
lsbtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsbtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
